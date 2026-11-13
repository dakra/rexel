;;; rexel.el --- Rex multiplexer client for ghostel -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Daniel Kraus <daniel@kraus.my>

;; Author: Daniel Kraus <daniel@kraus.my>
;; URL: https://github.com/dakra/rexel
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1") (ghostel "0.65.0") (websocket "1.15"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Attach to Rex (https://www.superlogical.com/rex) sessions and show each
;; terminal block as a ghostel buffer.
;;
;; Rex streams a terminal as a libghostty snapshot followed by the raw
;; PTY bytes.  Both go straight into ghostel's terminal engine, so
;; scrollback, search, copy mode and key encoding behave as in a local
;; terminal.  Keystrokes travel back over Rex's control connection.
;;
;; `rexel-attach' picks a session and `rexel-new-session' creates one.
;; `rexel' shows one session of its own, as `ghostel' shows a terminal,
;; and `rexel-project' in rexel-project.el the session of a project.
;; In a block buffer `rexel-prefix-key' (C-c C-r) prefixes the block and
;; session commands in `rexel-prefix-map'.
;;
;; A file dropped on a block buffer, pasted with `yank-media' or given to
;; `rexel-upload-file' is sent to the server's machine, and its path
;; there is typed into the terminal.  On this machine's server a local
;; file's own path is typed.
;;
;; Both connections are WebSockets, on a server's Unix socket or on its
;; network address, such as the one Rex's built-in Tailscale gives it.
;; Servers come from `rexel-program' and `rexel-hosts'.

;;; Code:

(require 'bindat)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'url-util)
(require 'ghostel)
(require 'websocket)

(defvar ghostel-annotation-function)
(declare-function dired-get-marked-files "dired")


;;; Customization

(defgroup rexel nil
  "Rex multiplexer client for ghostel."
  :group 'ghostel
  :prefix "rexel-")

(defcustom rexel-program "rex"
  "The rex command-line program, used to find and start servers."
  :type 'string)

(defcustom rexel-hosts nil
  "Rex servers to offer besides those `rexel-program' knows.
Each entry is (LABEL . ENDPOINT) or (LABEL ENDPOINT...), the endpoints
being tried in order.  ENDPOINT is a network address such as
\"studio.tail1234.ts.net\", \"https://HOST:PORT\" or \"http://HOST:PORT\",
or the absolute path of a Unix socket.  An address without a scheme is
reached over https only.  A host alone, without port or path, is tried
on its scheme's port, then on Rex's port 7297.  An entry replaces a
host of the same label known to Rex."
  :type '(alist :key-type string
                :value-type (choice string (repeat string))))

(defcustom rexel-autostart-server 'ask
  "Whether to run `rexel-start-server' when no server is found."
  :type '(choice (const :tag "Ask first" ask)
                 (const :tag "Always" t)
                 (const :tag "Never" nil)))

(defcustom rexel-session-label-function nil
  "Function turning a label given to `rexel-new-session' into the one used.
It receives the label, \"\" when none was given, in the buffer the
command runs from.  An empty or nil result lets the server choose one."
  :type '(choice (const :tag "None" nil) function))

(defcustom rexel-default-session "emacs"
  "Label of the session `rexel' shows."
  :type 'string)

(defcustom rexel-default-session-function nil
  "Function returning the label of the session `rexel' shows.
It is called without arguments in the buffer the command runs from.  A
nil result, like no function, means `rexel-default-session'.
`rexel-project-session-label' gives each project a session of its own."
  :type '(choice (const :tag "None" nil)
                 (function-item rexel-project-session-label)
                 function))

(defcustom rexel-tramp-hosts nil
  "TRAMP prefixes of the machines Rex servers run on.
An alist of (LABEL . PREFIX): the files of the server labelled LABEL are
those under the TRAMP prefix PREFIX, such as \"/ssh:studio:\".  In a
directory under PREFIX, `rexel' and `rexel-project' use that server, and
the buffers of its blocks follow their shells' directories there.
A server labelled with the host name of a TRAMP directory, or with that
name in its address, is found for the directory without an entry."
  :type '(alist :key-type (string :tag "Server label")
                :value-type (string :tag "TRAMP prefix")))

(defcustom rexel-attach-display 'ask
  "What `rexel-attach' shows of a session with several blocks.
`ask' reads a block, or a tab whose layout to show, from a list.
`focused' shows the active tab's focused block.  `layout' shows the
active tab's layout.  A block is shown with `pop-to-buffer', which keeps
the other windows; a tab with a split rebuilds the selected frame's
windows as `rexel-sync-layout' does, and a tab of one block shows it as
a block.  A session of one block is shown without asking."
  :type '(choice (const :tag "Ask for a block or a tab" ask)
                 (const :tag "The focused block" focused)
                 (const :tag "The active tab's layout" layout)))

(defcustom rexel-kill-buffer-action 'close
  "What killing a block buffer does to its block on the server.
`close' closes the block, which ends its program, and asks first when
the block runs a command.  `keep' leaves the block running; its buffer
returns with the session's next change.  `ask' asks which of the two to
do.  A buffer without a connection to its server leaves the block
running."
  :type '(choice (const :tag "Close the block" close)
                 (const :tag "Keep the block running" keep)
                 (const :tag "Ask" ask)))

(defcustom rexel-destroy-empty-sessions t
  "Whether a session is destroyed when its last block closes.
This holds for the sessions rexel is attached to, whatever closes the
block: a killed buffer, `rexel-close-block', a shell that exits, or
another Rex client.  A session with a block outside of every tab is not
empty.  With nil the session stays on the server without a tab."
  :type 'boolean)

(defvar rexel-prefix-map)
(defvar rexel-block-mode-map)

(defun rexel--key (key)
  "Return KEY, a string in `kbd' syntax or a key vector, as a key."
  (if (stringp key) (kbd key) key))

(defcustom rexel-prefix-key "C-c C-r"
  "Prefix key for `rexel-prefix-map' in block buffers.
A string in `kbd' syntax or a key vector."
  :type 'key-sequence
  :set (lambda (symbol key)
         (when (boundp 'rexel-block-mode-map)
           (define-key rexel-block-mode-map
                       (rexel--key (default-value symbol)) nil)
           (define-key rexel-block-mode-map (rexel--key key) rexel-prefix-map))
         (set-default symbol key)))


;;; Server connection

(defvar rexel--connect-timeout 10
  "Seconds a connection may take to open its WebSocket or answer a ping.
A write gets this long plus the time its size takes; see
`rexel--watched-write'.")

(defvar rexel--keepalive-interval 30
  "Seconds a connection may stay silent before it is pinged.")

(defvar rexel--keepalive-timer nil
  "Timer of the next `rexel--keepalive' run, or nil.")

(defvar rexel--default-port 7297
  "Port a Rex server listens on unless told another.")

(defconst rexel--version (package-get-version)
  "This package's version, sent in the hello.")

(defconst rexel--api-revision 11
  "Control API revision this client speaks.")

(cl-defstruct (rexel--conn (:constructor rexel--make-conn))
  "One control connection to a Rex server.

HOST is the server's label, nil for this machine's server.
ENDPOINTS are its addresses, each a Unix socket path or an http or https URL.
ENDPOINT is the one that last answered, and
UNTRIED are those left to try while a connection is being opened.

CLIENT-ID is the client's ID from the welcome, or t if it had none, and
nil unless the connection is open and welcomed.
TOKEN is the reconnect token of the latest welcome.
ON-READY are the callbacks to call with the connection at its next welcome.

DELAY is non-nil from the loss of the connection until the next
welcome, which then attaches the sessions again: the seconds the next
attempt to be scheduled waits.
TIMER is the timer of a scheduled attempt.

SESSIONS is an alist from the ID of an attached session to its label,
REVISIONS one to the revision of its view last applied, and
REFRESH-TIMERS one to the timer of its pending view refresh.
ATTACHING are the IDs of the sessions whose `session.attach' after a redial
is unanswered: the server refuses their typing and sizing until then.
BLOCKS maps a block ID to its buffer.

PENDING maps a request ID to the callback for its response.
CLOSING are the IDs of the blocks whose `block.close' is unanswered.
BUSY has a session's ID once for each unanswered request of
`rexel--busy-request' naming it.

TRAMP is the TRAMP prefix of the remote directory the server was last
used for, see `rexel--connect-directory'."
  ws endpoints endpoint untried host client-id token timer delay attaching
  closing busy tramp
  (next-id 0)
  (pending (make-hash-table :test #'equal))
  (sessions nil)
  (revisions nil)
  (blocks (make-hash-table :test #'equal))
  (on-ready nil)
  (refresh-timers nil))

(defvar rexel--conns nil
  "Open server connections, one per host.")

(defvar-local rexel--conn nil
  "The server connection of this block buffer.")

(defun rexel--parse-json (string)
  "Parse JSON STRING into alists with symbol keys; null and false are nil."
  (json-parse-string string :object-type 'alist
                     :null-object nil :false-object nil))

(defun rexel--program ()
  "Return the path of `rexel-program' or of the macOS app's copy, or nil."
  (or (executable-find rexel-program)
      (executable-find
       "/Applications/Rex Beta.app/Contents/Helpers/Rex Server.rex/Contents/MacOS/rex")))

(defun rexel--rex-json (&rest args)
  "Return the parsed JSON output of `rexel-program' run with ARGS, or nil."
  (when-let* ((rex (rexel--program)))
    (with-temp-buffer
      (when (zerop (apply #'call-process rex nil t nil
                          "--autostart=false" "--timeout" "2s" args))
        (ignore-errors (rexel--parse-json (buffer-string)))))))

(defun rexel--network-endpoint-p (endpoint)
  "Return non-nil if ENDPOINT is an http or https URL, not a socket path."
  (let ((case-fold-search nil))
    (string-match-p "\\`https?://" endpoint)))

(defun rexel--normalize-endpoint (address)
  "Return the server ADDRESS as a socket path or an http or https URL.
A unix:// URL or an absolute file name gives the socket path; an
address without a scheme gives an https URL.  Return nil for anything else,
such as another scheme or a URL with user information or a query."
  (let ((case-fold-search t)
        (host-and-path "\\([^/@?#[:space:]]+\\(?:/[^?#[:space:]]*\\)?\\)\\'"))
    (cond ((not (stringp address)) nil)
          ((string-match "\\`unix://\\(.*\\)\\'" address)
           (let ((path (url-unhex-string (match-string 1 address))))
             (and (file-name-absolute-p path) path)))
          ((file-name-absolute-p address) (expand-file-name address))
          ((or (string-match (concat "\\`\\(https?\\)://" host-and-path) address)
               (and (not (string-search "://" address))
                    (string-match (concat "\\`\\(\\)" host-and-path) address)))
           (let ((scheme (match-string 1 address))
                 (rest (match-string 2 address)))
             (concat (if (string-empty-p scheme) "https" (downcase scheme)) "://"
                     (replace-regexp-in-string "/+\\'" "" rest)))))))

(defun rexel--with-default-port (endpoint)
  "Return the endpoints to try for ENDPOINT, itself first.
A URL without port and path is tried on `rexel--default-port' too."
  (let ((url (and (rexel--network-endpoint-p endpoint)
                  (url-generic-parse-url endpoint))))
    (if (and url (not (url-portspec url)) (string-empty-p (url-filename url)))
        (list endpoint (format "%s:%d" endpoint rexel--default-port))
      (list endpoint))))

(defun rexel--endpoints (addresses)
  "Return the endpoints to try for the server ADDRESSES, in order.
Unusable addresses and repeated endpoints are left out."
  (delete-dups
   (mapcan #'rexel--with-default-port
           (delq nil (mapcar #'rexel--normalize-endpoint addresses)))))

(defun rexel--socket-live-p (path)
  "Return non-nil if something accepts connections on the Unix socket PATH."
  (ignore-errors
    (delete-process (make-network-process :name "rexel-probe" :family 'local
                                          :service path :noquery t))
    t))

(defun rexel--hosts ()
  "Return the servers to attach to as (LABEL . ENDPOINTS) pairs.
ENDPOINTS are Unix socket paths or http or https URLs, in the order to
try them.  This machine's server has label nil and is listed when its
socket accepts connections.  Hosts without a usable address are left out."
  (let* ((status (alist-get 'endpoint (rexel--rex-json "server" "status" "--json")))
         (local (cond ((and status (string-prefix-p "unix://" status))
                       (rexel--normalize-endpoint status))
                      ((eq system-type 'darwin)
                       (expand-file-name "~/Library/Application Support/rex/server.sock"))
                      (t "/run/rex/server.sock")))
         (mine (mapcar (lambda (h)
                         (cons (car h) (rexel--endpoints (ensure-list (cdr h)))))
                       rexel-hosts))
         (hosts (mapcar (lambda (h)
                          (cons (alist-get 'label h)
                                (rexel--endpoints (alist-get 'endpoints h))))
                        (alist-get 'hosts (rexel--rex-json "hosts" "list" "--json")))))
    (append (and (rexel--socket-live-p local) (list (list nil local)))
            (seq-filter (lambda (h) (and (stringp (car h)) (cdr h)))
                        (append mine
                                (seq-remove (lambda (h) (assoc-string (car h) mine t))
                                            hosts))))))

(defun rexel--ws-url (endpoint path)
  "Return the WebSocket URL of PATH on the server at ENDPOINT."
  (concat (if (rexel--network-endpoint-p endpoint)
              (concat "ws" (substring endpoint (length "http")))
            "ws://rex")
          path))

(defun rexel--open-network (endpoint)
  "Return a process connecting to the URL ENDPOINT, without waiting for it.
The name lookup can block.  Signal an error if the connection fails at
once."
  (let* ((url (url-generic-parse-url endpoint))
         (proc (open-network-stream
                (format "rexel %s" endpoint) nil
                (string-trim (url-host url) "\\[" "\\]") (url-port url)
                :type (if (equal (url-type url) "https") 'tls 'plain)
                :nowait t :coding 'binary)))
    (when (eq (process-status proc) 'failed)
      (delete-process proc)
      (error "Cannot connect to %s" endpoint))
    proc))

(defun rexel--ws-open (endpoint path protocol on-open on-message on-close)
  "Open a WebSocket to PATH on the server at ENDPOINT asking for PROTOCOL.
ENDPOINT is a Unix socket path or an http or https URL.  ON-OPEN,
ON-MESSAGE and ON-CLOSE are `websocket-open' callbacks.
`websocket-open' cannot dial a Unix socket and blocks until a network
connection is open, so the connection is made here and handed to
websocket.el for the handshake and framing."
  (let* ((url (rexel--ws-url endpoint path))
         (key (websocket-genkey))
         (protocols (list protocol))
         (coding-system-for-read 'binary)
         (coding-system-for-write 'binary)
         (network (rexel--network-endpoint-p endpoint))
         (conn (if network
                   (rexel--open-network endpoint)
                 (make-network-process :name (format "rexel %s" url) :buffer nil
                                       :family 'local :service endpoint
                                       :coding 'binary)))
         (ws (websocket-inner-create
              :conn conn :url url :protocols protocols
              ;; websocket.el also calls this after a refused handshake.
              :on-open (lambda (ws) (when (websocket-openp ws) (funcall on-open ws)))
              :on-message (lambda (ws frame)
                            (rexel--unwatched
                             (lambda () (funcall on-message ws frame))))
              :on-close on-close
              :on-error #'rexel--ws-error
              :accept-string (websocket-calculate-accept key)))
         (sentinel (websocket-sentinel url conn key protocols nil nil nil)))
    (process-put conn :websocket ws)
    (process-put conn :last-read (float-time))
    (unless (memq rexel--keepalive-timer timer-list)
      (setq rexel--keepalive-timer
            (run-at-time rexel--keepalive-interval nil #'rexel--keepalive)))
    (set-process-filter conn #'rexel--ws-filter)
    ;; A network connection is still opening here; its handshake is sent
    ;; once the sentinel reports it open.  A peer that is gone by then
    ;; fails the send and closes the process.
    (set-process-sentinel
     conn (lambda (process change)
            (when (eq (process-status process) 'open)
              (ignore-errors
                (websocket-ensure-handshake url conn key protocols nil nil nil)))
            (funcall sentinel process change)))
    (set-process-query-on-exit-flag conn nil)
    (websocket-ensure-handshake url conn key protocols nil nil nil)
    ;; Emacs runs no sentinel for some failed connections, and a server
    ;; may accept one and never answer.
    (run-at-time rexel--connect-timeout nil
                 (lambda ()
                   ;; Emacs waiting on something else leaves the answer unread.
                   (when (and (eq (websocket-ready-state ws) 'connecting)
                              (process-live-p conn))
                     (accept-process-output conn 0 nil 1))
                   (when (eq (websocket-ready-state ws) 'connecting)
                     (delete-process conn))))
    ws))

(defun rexel--ws-filter (process output)
  "Pass PROCESS OUTPUT to websocket.el once a whole frame is buffered.
`websocket-outer-filter' copies all buffered input on every read, so
a large frame arriving in many reads would cost quadratic time."
  (process-put process :last-read (float-time))
  (let* ((ws (process-get process :websocket))
         (head (websocket-inflight-input ws))
         (queued (cons output (process-get process :queued)))
         (have (+ (length head) (apply #'+ (mapcar #'length queued))))
         ;; Server frames are unmasked: 1 opcode byte, length, payload.
         (need (and head (eq (websocket-ready-state ws) 'open)
                    (catch 'websocket-incomplete-frame
                      (let ((len (websocket-get-payload-len
                                  (substring head 1 (min 10 (length head))))))
                        (+ 1 (cdr len) (car len)))))))
    (if (and need (< have need))
        (process-put process :queued queued)
      (process-put process :queued nil)
      (condition-case err
          ;; websocket.el answers a ping from here.
          (rexel--watched-write
           process nil
           (lambda ()
             (websocket-outer-filter ws (apply #'concat (nreverse queued)))))
        ;; Errors matter only while the connection lives: a message handler
        ;; can close WS while a ping waits behind the message, and a stalled
        ;; pong deletes the process.
        (error (when (process-live-p process)
                 (signal (car err) (cdr err))))))))

(defun rexel--keepalive ()
  "Probe every connection silent for `rexel--keepalive-interval'.
Run again when the next connection falls due, until none is left."
  (let ((now (float-time))
        next)
    (dolist (process (process-list))
      (when-let* ((read (process-get process :last-read))
                  ((process-live-p process)))
        (let ((due (+ read rexel--keepalive-interval)))
          (when (<= due now)
            (rexel--probe (process-get process :websocket))
            (setq due (+ now rexel--keepalive-interval)))
          (setq next (min due (or next due))))))
    (when rexel--keepalive-timer
      (cancel-timer rexel--keepalive-timer))
    (setq rexel--keepalive-timer
          (and next (run-at-time (max 1 (- next now)) nil #'rexel--keepalive)))))

(defun rexel--probe (ws)
  "Ping the open WS and close its connection if the server stays silent.
A connection has one probe at a time.  Closing it runs the on-close
callback of WS, which redials or resumes.  A write in progress on the
connection is watched itself, and a ping would only queue behind it."
  (let ((process (websocket-conn ws))
        (sent (float-time)))
    (when (and (eq (websocket-ready-state ws) 'open)
               (not (process-get process :probe))
               (not (rexel--write-earliest process)))
      (condition-case nil
          (progn
            ;; The server closes a connection on a ping without payload.
            (rexel--watched-write
             process nil
             (lambda ()
               (websocket-send ws (make-websocket-frame
                                   :opcode 'ping :payload "rexel" :completep t)))
             0)
            (process-put
             process :probe
             (run-at-time rexel--connect-timeout nil
                          (lambda ()
                            (process-put process :probe nil)
                            (when (process-live-p process)
                              ;; Emacs waiting on another process leaves
                              ;; the answer unread.
                              (when (< (process-get process :last-read) sent)
                                (accept-process-output process 0 nil 1))
                              (when (< (process-get process :last-read) sent)
                                (delete-process process)))))))
        (error (delete-process process))))))

(defun rexel--ws-error (ws type err)
  "Report ERR signalled while running the TYPE callback of WS."
  (if (equal err '(websocket-received-error-http-response 403))
      (message "rexel: %s: the server does not admit this user or device"
               (websocket-url ws))
    (message "rexel: %s callback on %s: %S" type (websocket-url ws) err)))

(defun rexel--send (conn object)
  "Send OBJECT as one JSON text message on CONN."
  (let ((ws (rexel--conn-ws conn))
        (text (json-serialize object)))
    (rexel--watched-write (websocket-conn ws) nil
                          (lambda () (websocket-send-text ws text))
                          (string-bytes text))))

(defun rexel--error-string (err)
  "Format a server ERR object."
  (format "%s (%s)" (alist-get 'message err) (alist-get 'code err)))

(defun rexel--request (conn method payload &optional callback)
  "Call METHOD with PAYLOAD on CONN.
CALLBACK receives the result and the error object, one of them nil.
Without CALLBACK an error is shown as a message."
  (let ((id (number-to-string (cl-incf (rexel--conn-next-id conn)))))
    (puthash id (or callback
                    (lambda (_result err)
                      (when err
                        (message "rexel: %s: %s" method (rexel--error-string err)))))
             (rexel--conn-pending conn))
    (rexel--send conn `((type . "request") (id . ,id) (method . ,method)
                        ,@(when payload `((payload . ,payload)))))))

(defun rexel--busy-request (conn sid method payload callback)
  "Call METHOD with PAYLOAD on CONN as `rexel--request' does.
Until CALLBACK returns, session SID is not destroyed for being empty."
  (rexel--request
   conn method payload
   (lambda (result err)
     (unwind-protect
         (funcall callback result err)
       (setf (rexel--conn-busy conn)
             (cl-delete sid (rexel--conn-busy conn) :test #'equal :count 1)))))
  (push sid (rexel--conn-busy conn)))

(defun rexel--session-request (conn sid method callback)
  "Call METHOD for session SID on CONN, and CALLBACK with its result.
An error is shown as a message, unless it says that the session is gone."
  (rexel--request
   conn method `((session_id . ,sid))
   (lambda (result err)
     (cond ((null err) (funcall callback result))
           ((equal (alist-get 'code err) "session_not_found"))
           (t (message "rexel: %s: %s" method (rexel--error-string err)))))))

(defun rexel--connect (host callback)
  "Call CALLBACK with a ready connection to HOST, opening one if needed.
HOST is a (LABEL . ENDPOINTS) pair as `rexel--hosts' returns them."
  (let* ((endpoints (cdr host))
         (conn (seq-find (lambda (c) (equal (rexel--conn-endpoints c) endpoints))
                         rexel--conns)))
    (cond
     ((and conn (websocket-openp (rexel--conn-ws conn))
           (rexel--conn-client-id conn))
      (funcall callback conn))
     (conn
      (push callback (rexel--conn-on-ready conn))
      ;; Unless opening, or open and not yet welcomed.
      (unless (process-live-p (websocket-conn (rexel--conn-ws conn)))
        (setf (rexel--conn-delay conn) nil)
        (rexel--reconnect conn)))
     (t
      (unless (seq-some (lambda (e) (or (rexel--network-endpoint-p e) (file-exists-p e)))
                        endpoints)
        (user-error "Rex server socket %s does not exist" (car endpoints)))
      (setq conn (rexel--make-conn :endpoints endpoints :host (car host)
                                   :on-ready (list callback)))
      (rexel--dial conn)
      (push conn rexel--conns)))))

(defun rexel--dial (conn &optional endpoints)
  "Open CONN's control connection and send the hello.
Try ENDPOINTS in order until a server answers one with a welcome.
By default these are all of CONN's, the one that last answered first.
Signal the last error if none can be dialled."
  (let* ((last (rexel--conn-endpoint conn))
         (endpoints (or endpoints
                        (append (and last (list last))
                                (remove last (rexel--conn-endpoints conn)))))
         ws err)
    (while (and endpoints (not ws))
      (let ((endpoint (pop endpoints)))
        (condition-case e
            (setq ws (rexel--ws-open
                      endpoint "/api/control" "rex.control.v1"
                      (lambda (_ws)
                        (setf (rexel--conn-endpoint conn) endpoint)
                        (rexel--send-hello conn))
                      (lambda (_ws frame)
                        (rexel--on-control-message conn (websocket-frame-text frame)))
                      (lambda (ws)
                        (when (eq ws (rexel--conn-ws conn))
                          (rexel--on-dial-close conn)))))
          (error (setq err e)))))
    (setf (rexel--conn-untried conn) endpoints)
    (unless ws
      (signal (car err) (cdr err)))
    (setf (rexel--conn-ws conn) ws)))

(defun rexel--send-hello (conn)
  "Send the hello that opens CONN's control connection."
  (rexel--send conn `((type . "hello")
                      (api_revision . ,rexel--api-revision)
                      ,@(when (rexel--conn-token conn)
                          `((reconnect_token . ,(rexel--conn-token conn))))
                      (client . ((kind . "other")
                                 (name . "rexel")
                                 (version . ,(or rexel--version "unknown")))))))

(defun rexel--on-dial-close (conn)
  "Handle the close of CONN's control connection.
If it closed before a welcome, dial the endpoints not yet tried."
  (let ((untried (rexel--conn-untried conn)))
    (unless (and untried (ignore-errors (rexel--dial conn untried)))
      (rexel--on-control-close conn))))

(defun rexel--conn-name (conn)
  "Return CONN's host label or endpoint for messages."
  (or (rexel--conn-host conn) (rexel--conn-endpoint conn)
      (car (rexel--conn-endpoints conn))))

(defun rexel--ready-p (conn)
  "Return non-nil when CONN is open and has been welcomed."
  (and conn (rexel--conn-client-id conn)))

(defun rexel--reconnect (conn)
  "Dial CONN again after its delay, doubling it up to 30 seconds.
Once no session or callback waits for CONN, it is forgotten instead."
  (let ((delay (or (rexel--conn-delay conn) 1)))
    (when (rexel--conn-timer conn)
      (cancel-timer (rexel--conn-timer conn)))
    (setf (rexel--conn-delay conn) (min 30 (* 2 delay))
          (rexel--conn-timer conn)
          (run-at-time delay nil
                       (lambda ()
                         (setf (rexel--conn-timer conn) nil)
                         (if (or (rexel--conn-sessions conn)
                                 (rexel--conn-on-ready conn))
                             (condition-case nil
                                 (rexel--dial conn)
                               (error
                                (rexel--fail-waiters conn)
                                (rexel--reconnect conn)))
                           (setq rexel--conns (delq conn rexel--conns))))))))

(defun rexel--fail-waiters (conn)
  "Drop the callbacks waiting for CONN, saying it cannot be reached.
Return non-nil if there were any."
  (when (rexel--conn-on-ready conn)
    (setf (rexel--conn-on-ready conn) nil)
    (message "rexel: cannot reach %s" (rexel--conn-name conn))
    t))

(defun rexel--on-control-message (conn text)
  "Dispatch the control message TEXT received on CONN."
  (let ((msg (rexel--parse-json text)))
    (pcase (alist-get 'type msg)
      ("welcome"
       (setf (rexel--conn-untried conn) nil)
       (let ((protocol (alist-get 'protocol msg))
             (revision (or (alist-get 'api_revision msg) 0)))
         (if (and (equal protocol "rex.control.v1")
                  (eql revision rexel--api-revision))
             (let ((client (alist-get 'client msg)))
               (setf (rexel--conn-client-id conn) (or (alist-get 'id client) t)
                     (rexel--conn-token conn) (alist-get 'reconnect_token client))
               (when (rexel--conn-delay conn)
                 (setf (rexel--conn-delay conn) nil)
                 (message "rexel: %s to %s"
                          (if (alist-get 'resumed client)
                              "reconnected"
                            "re-attached as a new client")
                          (rexel--conn-name conn))
                 (rexel--reattach conn))
               (let ((callbacks (nreverse (rexel--conn-on-ready conn))))
                 (setf (rexel--conn-on-ready conn) nil)
                 (dolist (callback callbacks) (funcall callback conn))))
           (websocket-close (rexel--conn-ws conn))
           (message "rexel: server speaks %s revision %s, rexel needs %s revision %d; update Rex"
                    protocol revision "rex.control.v1" rexel--api-revision))))
      ("response"
       (let* ((id (alist-get 'id msg))
              (callback (gethash id (rexel--conn-pending conn))))
         (remhash id (rexel--conn-pending conn))
         (when callback
           (funcall callback (alist-get 'payload msg) (alist-get 'error msg)))))
      ("event" (rexel--on-event conn (alist-get 'payload msg))))))

(defun rexel--on-control-close (conn)
  "Redial CONN if sessions are attached to it, else forget it.
Uploads fail either way.  Block streams run on their own connections and
stay up while CONN redials; without sessions they end, and the buffers
stay for reading until `rexel-attach' reuses them."
  (pcase-dolist (`(,_sid . ,timer) (rexel--conn-refresh-timers conn))
    (cancel-timer timer))
  (setf (rexel--conn-refresh-timers conn) nil
        (rexel--conn-client-id conn) nil
        (rexel--conn-closing conn) nil
        (rexel--conn-busy conn) nil)
  (clrhash (rexel--conn-pending conn))
  (dolist (buf (buffer-list))
    (when (and (eq (buffer-local-value 'rexel--conn buf) conn)
               (buffer-local-value 'rexel--uploads buf))
      (rexel--upload-fail buf "control connection lost")))
  (if (rexel--conn-sessions conn)
      (progn
        (setf (rexel--conn-revisions conn) nil)
        (unless (or (rexel--fail-waiters conn) (rexel--conn-delay conn))
          (message "rexel: connection to %s lost, reconnecting"
                   (rexel--conn-name conn)))
        (rexel--reconnect conn))
    (setq rexel--conns (delq conn rexel--conns))
    (dolist (buf (buffer-list))
      (when (eq (buffer-local-value 'rexel--conn buf) conn)
        (rexel--end-stream buf "control connection closed")
        (with-current-buffer buf (setq rexel--conn nil))))
    (unless (rexel--fail-waiters conn)
      (message "rexel: control connection to %s closed" (rexel--conn-name conn)))))

(defvar rexel--exit-timeout 1
  "Seconds an exiting Emacs waits for a connection to take its close frame.")

(defun rexel--leave-servers ()
  "Send every control connection a close frame, for `kill-emacs-hook'.
The server then ends this client at once; one whose connection drops
stays attached to its sessions for a minute.  A connection being written
to is left to drop, as the frame would wait behind that write."
  (dolist (conn rexel--conns)
    (ignore-errors
      (let* ((ws (rexel--conn-ws conn))
             (process (websocket-conn ws))
             (rexel--connect-timeout rexel--exit-timeout))
        (when (and (eq (websocket-ready-state ws) 'open)
                   (not (rexel--write-earliest process)))
          ;; The server closes the connection in answer, which must not
          ;; redial.
          (setf (websocket-on-close ws) #'ignore)
          (rexel--watched-write
           process nil
           ;; Status 1001, going away, under a zero mask key.  websocket.el
           ;; sends a close frame unmasked, which the server takes for a
           ;; drop.
           (lambda ()
             (process-send-string
              process (unibyte-string #x88 #x82 0 0 0 0 3 233)))
           0))))))

(add-hook 'kill-emacs-hook #'rexel--leave-servers)

(defun rexel--on-event (conn event)
  "Handle EVENT received on CONN.
Handlers must tolerate events that arrive before the `session.attach'
response or repeat a change the view already has."
  (let ((payload (alist-get 'payload event)))
    (pcase (alist-get 'type event)
      ("session_event"
       (let* ((sid (alist-get 'session_id payload))
              (inner (alist-get 'event payload))
              (inner-payload (alist-get 'payload inner)))
         (pcase (alist-get 'type inner)
           ("block_closed"
            (when-let* ((buf (gethash (alist-get 'block_id inner-payload)
                                      (rexel--conn-blocks conn))))
              (rexel--discard-buffer buf))
            (when rexel-destroy-empty-sessions
              (rexel--destroy-if-empty conn sid)))
           ("session_view_changed"
            (when (assoc sid (rexel--conn-sessions conn))
              (rexel--schedule-view-refresh conn sid)))
           ("block_event"
            (rexel--on-block-event conn inner-payload))
           ("session_destroyed" (rexel--forget-session conn sid)))))
      ("session_destroyed"
       (rexel--forget-session conn (alist-get 'session_id payload))))))


;;; Watched writes

(cl-defstruct (rexel--write (:constructor rexel--make-write) (:copier nil))
  "A write that may wait for room in the socket of PROCESS.
ON-STALL gives it up, TIMER watches it, and it has until DEADLINE.  SIZE
is the bytes it writes, nil for a filter, which may write none."
  process on-stall timer deadline size)

(defvar rexel--writes nil
  "The `rexel--write's in progress, innermost first.
`process-send-string' returns only when the socket took everything, and
Emacs reads no keys meanwhile.  Timers and filters run during that wait,
so writes nest in it, and a write can only return after those inside it.
The symbol `handler' stands for a message handler running.")

(defvar rexel--writes-overdue nil
  "Writes past their first deadline that wait for a write to another socket.
Upload chunks hold back while there are any, so that these can return.
Inside a message handler it starts empty: the writes around the handler
cannot return before it does.")

(defvar rexel--handler-began nil
  "When the running message handler began or last had its time counted.")

(defconst rexel--write-rate 16384
  "Bytes per second the slowest working link is expected to take.")

(defun rexel--write-earliest (process)
  "Return the earliest write in progress to the socket of PROCESS, if any.
A filter, which has no SIZE, does not count."
  (cl-find-if (lambda (write)
                (and (rexel--write-p write)
                     (rexel--write-size write)
                     (eq (rexel--write-process write) process)))
              rexel--writes :from-end t))

(defun rexel--watched-write (process on-stall write &optional size)
  "Call WRITE, which writes SIZE bytes to PROCESS, and return its value.
A write not done within `rexel--connect-timeout', plus the time SIZE takes
at `rexel--write-rate', is given up by calling ON-STALL or, without it, by
deleting PROCESS.  A write queued behind an earlier one to the same socket
adds the time of its SIZE to that write's deadline."
  (let* ((seconds (/ (or size 0) (float rexel--write-rate)))
         (earliest (rexel--write-earliest process))
         (entry (rexel--make-write
                 :process process :on-stall on-stall :size size
                 :deadline (+ (float-time) rexel--connect-timeout seconds)))
         (rexel--writes (progn (rexel--count-handler-time)
                               (cons entry rexel--writes))))
    (when earliest
      (cl-incf (rexel--write-deadline earliest) seconds))
    (setf (rexel--write-timer entry)
          (run-at-time (+ rexel--connect-timeout seconds) 1
                       #'rexel--write-stalled entry))
    (unwind-protect
        (funcall write)
      (cancel-timer (rexel--write-timer entry))
      (setq rexel--writes-overdue (delq entry rexel--writes-overdue)))))

(defun rexel--count-handler-time ()
  "Add the time of the innermost message handler to the writes around it.
They cannot return before it does.  The handler's clock then restarts."
  (when (eq (car rexel--writes) 'handler)
    (let ((now (float-time)))
      (dolist (write (seq-take-while #'rexel--write-p (cdr rexel--writes)))
        (cl-incf (rexel--write-deadline write) (- now rexel--handler-began)))
      (setq rexel--handler-began now))))

(defun rexel--unwatched (function)
  "Call FUNCTION, a message handler, marked on `rexel--writes'.
It runs inside the filter's watched write and may take as long as it
likes, as when it prompts: no write counts as stalled while it is
innermost, and its time extends the writes around it."
  (let ((rexel--writes (cons 'handler rexel--writes))
        (rexel--handler-began (float-time))
        (rexel--writes-overdue nil))
    (unwind-protect
        (funcall function)
      (rexel--count-handler-time))))

(defun rexel--write-blocking ()
  "Return the write that everything in progress waits for, if there is one.
That is the innermost write, or the earliest to the same socket, which
it queues behind.  With a message handler innermost it is nil."
  (let ((innermost (car rexel--writes)))
    (when (rexel--write-p innermost)
      (or (rexel--write-earliest (rexel--write-process innermost))
          innermost))))

(defun rexel--write-stalled (write)
  "Give up WRITE if it is what the others wait for and out of time.
Runs every second from its first deadline on.  A write held up by a
write to another socket goes on `rexel--writes-overdue', which holds
upload chunks back so that it can return; one waiting for a message
handler does not, as it cannot return before the handler."
  (let ((blocking (rexel--write-blocking)))
    (cond
     ((and (eq write blocking) (>= (float-time) (rexel--write-deadline write)))
      (if (rexel--write-on-stall write)
          (funcall (rexel--write-on-stall write))
        (delete-process (rexel--write-process write))))
     ((and blocking
           (not (eq (rexel--write-process write)
                    (rexel--write-process blocking)))
           (not (memq 'handler (cl-ldiff rexel--writes
                                         (memq write rexel--writes)))))
      (cl-pushnew write rexel--writes-overdue :test #'eq)))))


;;; Session views and block buffers

(defconst rexel--terminal-creator "com.superlogical.terminal"
  "Creator name of Rex terminal blocks.")

(defvar-local rexel--session-id nil
  "ID of the Rex session this block buffer belongs to.")

(defvar-local rexel--block-id nil
  "ID of the Rex block shown in this buffer.")

(cl-defstruct (rexel--stream (:constructor rexel--make-stream) (:copier nil))
  "One data connection streaming a block's terminal.
WS is its WebSocket and MARKER the prefix of its control messages,
including the channel nonce.  SNAPSHOT holds the complete snapshot chunks
received so far and TAIL the chunks of the incomplete record after them,
both newest first; TAIL is walked again once it holds NEED bytes.
PENDING holds live bytes that may begin a control message.
TOKEN is the reconnect token of the latest WELCOME, RECEIVED the number
of stream bytes received over all connections, and RESUME the state to
return to when a RESUME is welcomed."
  ws marker snapshot tail (need 10) (pending "") token (received 0) resume)

(defvar-local rexel--stream nil
  "This block's current `rexel--stream', or nil.")

(defvar-local rexel--data-state nil
  "Where the data connection is: `welcome', `snapshot', `live' or `closed'.
Nil until the answer to `data.connect'.")

(defvar-local rexel--requested-size nil
  "Last (COLS . ROWS) requested from the server for this block.")

(defvar-local rexel--size-take nil
  "Non-nil when the next size request takes ownership of the block.")

(defvar-local rexel--size-inflight nil
  "The connection a size request is in flight on, or nil.")

(defvar-local rexel--theme-inflight nil
  "The connection a theme request is in flight on, or nil.")

(defvar-local rexel--out-pending nil
  "Typed bytes not yet sent to the server.")

(defvar-local rexel--out-inflight nil
  "The connection a `terminal.write' is in flight on, or nil.")

(defvar-local rexel--upload-queue nil
  "Local files waiting to be uploaded to this block, in order.")

(defvar-local rexel--uploads nil
  "The `rexel--upload's of the batch in progress, in the order of its paths.")

(defvar-local rexel--shell-pid nil
  "Pid of the block's shell, from the processes Rex reports.")

(defvar-local rexel--shell-program nil
  "Path and argv0 of the block's shell; they change when the shell execs.")

(defvar-local rexel--owner nil
  "ID of the client that owns this block and so sizes it, nil when none does.")

(defvar-local rexel--stale-prompt nil
  "The value of `ghostel-last-prompt' when the shell last execed.
OSC 133 marks count again once a prompt was marked after it.")

(defvar rexel--keep-blocks nil
  "Non-nil while rexel itself kills block buffers, which closes no block.")

(defvar-local rexel--close-on-kill 'unasked
  "Whether killing this buffer closes its block: t, nil or `unasked'.
`rexel--kill-buffer-query' decides.")

(defvar rexel--kill-declined t
  "The sessions with a block the user chose to keep, while bound to a list.
`rexel--kill-buffer-query' adds a session's ID when its answer keeps a
block of it.")

(defconst rexel--same-window
  '(display-buffer-same-window (inhibit-same-window . nil) (category . comint))
  "The `display-buffer' action ghostel's own commands show a terminal with.")

(defun rexel--attach (conn sid callback)
  "Attach CONN to session SID, apply its view and call CALLBACK with it."
  (rexel--request
   conn "session.attach" `((session_id . ,sid))
   (lambda (result err)
     (if err
         (message "rexel: session.attach: %s" (rexel--error-string err))
       (let ((view (alist-get 'view result)))
         (rexel--apply-view conn sid view)
         (funcall callback view))))))

(defun rexel--attach-session (conn sid &optional display)
  "Attach CONN to session SID and show it as DISPLAY says.
DISPLAY is a value of `rexel-attach-display'; nil shows the focused block."
  (rexel--attach conn sid
                 (lambda (view)
                   (rexel--show-attached conn sid view display))))

(defun rexel--with-session (conn sid callback)
  "Call CALLBACK with the view of session SID on CONN, attaching if needed.
The server answers nothing about a session that is being reattached."
  (cond ((member sid (rexel--conn-attaching conn))
         (message "rexel: the session is still being reattached"))
        ((assoc sid (rexel--conn-sessions conn))
         (rexel--with-view conn sid callback))
        (t (rexel--attach conn sid callback))))

(defun rexel--show-attached (conn sid view display)
  "Show session SID's VIEW on CONN as DISPLAY, a `rexel-attach-display' value."
  (let ((tab (rexel--active-tab view))
        (blocks (rexel--view-terminal-blocks view)))
    (cond
     ((null blocks)
      (message "rexel: Rex session %s has no terminal" (alist-get 'label view)))
     ((null (cdr blocks))
      (rexel--show-focused-block conn view))
     ((eq display 'ask)
      (rexel--read-attach-choice
       conn sid view
       (lambda (choice)
         (pcase choice
           (`(block . ,bid)
            (unless (rexel--pop-to-block conn bid)
              (message "rexel: the Rex block is gone")))
           (`(tab . ,tab)
            (rexel--tab-again conn sid (alist-get 'window_id tab)
                              (apply-partially #'rexel--show-tab conn sid)))))))
     ((and (eq display 'layout) (rexel--tab-block tab))
      (rexel--show-tab conn sid view tab))
     (t (rexel--show-focused-block conn view)))))

(defun rexel--prompt-later (function)
  "Call FUNCTION, which prompts, from a timer and ignore a quit out of it.
A message handler that prompts holds up the messages after it in the
same read, and a quit out of it drops them."
  (run-at-time 0 nil
               (lambda ()
                 (rexel--unwatched
                  (lambda () (condition-case nil (funcall function) (quit nil)))))))

(defun rexel--with-view (conn sid callback)
  "Fetch session SID's view on CONN, apply it and call CALLBACK with it.
A session detached meanwhile is left alone."
  (rexel--busy-request
   conn sid "session.view" `((session_id . ,sid))
   (lambda (view err)
     (cond
      (err (message "rexel: session.view: %s" (rexel--error-string err)))
      ((assoc sid (rexel--conn-sessions conn))
       (rexel--apply-view conn sid view)
       (funcall callback view))))))

(defun rexel--tab-again (conn sid tab-id callback)
  "Call CALLBACK with session SID's current view on CONN and tab TAB-ID in it."
  (rexel--with-view
   conn sid
   (lambda (view)
     (if-let* ((tab (seq-find (lambda (w) (equal (alist-get 'window_id w) tab-id))
                              (alist-get 'windows view))))
         (funcall callback view tab)
       (message "rexel: the Rex tab is gone")))))

(defun rexel--pop-to-block (conn bid &optional action)
  "Pop to the buffer of block BID on CONN and return it, if it has one.
ACTION is the `display-buffer' action to show it with."
  (let ((buf (gethash bid (rexel--conn-blocks conn))))
    (when (buffer-live-p buf)
      (pop-to-buffer buf action))))

(defun rexel--tab-layout (tab)
  "Return the layout of TAB's tiled blocks, or nil.
Floating layers hold their blocks above it and are left out."
  (alist-get 'layout (seq-first (alist-get 'layers tab))))

(defun rexel--tab-terminal-blocks (tab)
  "Return TAB's terminal blocks as (BLOCK . LAYER), the tiled layer's first."
  (let (blocks)
    (seq-doseq (layer (alist-get 'layers tab))
      (seq-doseq (block (alist-get 'blocks layer))
        (when (equal (alist-get 'creator_name block) rexel--terminal-creator)
          (push (cons block layer) blocks))))
    (nreverse blocks)))

(defun rexel--layer-minimized-p (layer)
  "Return non-nil when Rex hides LAYER's blocks."
  (equal (alist-get 'state layer) "minimized"))

(defun rexel--layer-note (layer)
  "Return \" (floating)\" or \" (minimized)\" for a LAYER above the layout, else \"\"."
  (cond ((rexel--layer-minimized-p layer) " (minimized)")
        ((equal (alist-get 'kind layer) "tiled") "")
        (t " (floating)")))

(defun rexel--tab-block (tab)
  "Return the ID of TAB's focused terminal block, else of its first one.
A block in a minimized layer comes last.  Nil when TAB has no terminal."
  (let ((blocks (rexel--tab-terminal-blocks tab))
        (focused (alist-get 'focused_block_id tab)))
    (when-let* ((entry (or (seq-find (lambda (b) (equal (alist-get 'block_id (car b))
                                                       focused))
                                     blocks)
                           (seq-find (lambda (b) (not (rexel--layer-minimized-p (cdr b))))
                                     blocks)
                           (car blocks))))
      (alist-get 'block_id (car entry)))))

(defun rexel--block-tab (view bid)
  "Return the tab of VIEW holding block BID in any of its layers, or nil."
  (seq-find (lambda (tab)
              (seq-some (lambda (layer)
                          (seq-some (lambda (b) (equal (alist-get 'block_id b) bid))
                                    (alist-get 'blocks layer)))
                        (alist-get 'layers tab)))
            (alist-get 'windows view)))

(defun rexel--view-block-owner (view bid)
  "Return the ID of the client owning block BID in VIEW, or nil."
  (seq-some (lambda (tab)
              (seq-some (lambda (entry)
                          (and (equal (alist-get 'block_id (car entry)) bid)
                               (alist-get 'owner (car entry))))
                        (rexel--tab-terminal-blocks tab)))
            (alist-get 'windows view)))

(defun rexel--show-tab (conn sid view tab)
  "Show TAB of session SID's VIEW on CONN.
A tab with a split gets the selected frame's windows rebuilt from its layout;
a tab of one block pops to it, and a tab without a layout to its focused block."
  (let ((layout (rexel--tab-layout tab)))
    (if (alist-get 'split layout)
        (rexel--layout-windows conn sid view tab)
      (unless (rexel--pop-to-block conn (or (alist-get 'block_id layout)
                                            (rexel--tab-block tab)))
        (message "rexel: Rex tab %s holds no terminal" (rexel--tab-name view tab))))))

(defun rexel--tab-label (tab)
  "Return the label TAB was given, or nil.
The server labels a tab without one \"Window\" and the end of its ID."
  (let ((label (alist-get 'label tab))
        (id (alist-get 'window_id tab)))
    (unless (or (member label '(nil ""))
                (and (>= (length id) 8)
                     (equal label (concat "Window " (substring id -8)))))
      label)))

(defun rexel--tab-name (view tab)
  "Return TAB's label, or for a tab without one its number in VIEW."
  (or (rexel--tab-label tab)
      (format "tab %d" (1+ (or (seq-position (alist-get 'windows view) tab #'eq)
                               0)))))

(defun rexel--view-block-names (view)
  "Return the (BLOCK-ID . NAME) of every terminal block in VIEW.
NAME is the label of the block's tab, or for a tab without one its
number.  A tab's blocks after its first get their number too, as in 2.3."
  (let ((names nil))
    (seq-do-indexed
     (lambda (tab i)
       (let ((name (or (rexel--tab-label tab) (number-to-string (1+ i))))
             (n 0))
         (pcase-dolist (`(,block . ,_) (rexel--tab-terminal-blocks tab))
           (push (cons (alist-get 'block_id block)
                       (if (zerop n) name (format "%s.%d" name (1+ n))))
                 names)
           (cl-incf n))))
     (alist-get 'windows view))
    (nreverse names)))

(defun rexel--home-relative (dir home)
  "Return DIR with a leading HOME directory written as ~."
  (let ((home (and home (file-name-as-directory home))))
    (cond ((null home) dir)
          ((equal (file-name-as-directory dir) home) "~")
          ((string-prefix-p home dir) (concat "~/" (substring dir (length home))))
          (t dir))))

(defun rexel--block-description (block process)
  "Describe BLOCK by the foreground program and directory in PROCESS.
PROCESS is a `terminal.process' report, or nil; without one, or before
the block's shell started, BLOCK's label describes it.  The program is
named by the base name of the first of its script, argv0 and name."
  (let* ((fg (alist-get 'foreground process))
         (program (seq-some (lambda (key) (alist-get key fg)) '(script argv0 name)))
         (cwd (alist-get 'cwd fg)))
    (if (or (null program) (member (alist-get 'name fg) '(nil "login" "rex")))
        (let ((label (alist-get 'label block)))
          (if (member label '(nil "")) "block" label))
      (concat (string-remove-prefix "-" (file-name-nondirectory program))
              (when cwd
                (concat " in " (rexel--home-relative cwd (alist-get 'home process))))))))

(defun rexel--attach-choices (view processes)
  "Return VIEW's terminal blocks and tabs as (NAME . CHOICE).
CHOICE is (block . BLOCK-ID) or (tab . TAB).  PROCESSES maps block IDs
to their `terminal.process' reports, which describe the blocks.  The
active tab's focused block comes first, the tabs last.  A block above its
tab's layout is marked floating or minimized.  A name that repeats gets
its block's ID or its tab's number appended."
  (let ((focused (alist-get 'focused_block_id (rexel--active-tab view)))
        blocks tabs)
    ;; Each entry is (NAME SUFFIX . CHOICE) until the names are unique.
    (seq-do-indexed
     (lambda (tab i)
       (let ((name (rexel--tab-name view tab)))
         (pcase-dolist (`(,block . ,layer) (rexel--tab-terminal-blocks tab))
           (let ((bid (alist-get 'block_id block)))
             (push `(,(format "%s: %s%s" name
                              (rexel--block-description
                               block (cdr (assoc bid processes)))
                              (rexel--layer-note layer))
                     ,(substring bid -6) block . ,bid)
                   blocks)))
         (push `(,(format "Layout of %s" name) ,(number-to-string (1+ i))
                 tab . ,tab)
               tabs)))
     (alist-get 'windows view))
    (setq blocks (nreverse blocks))
    (when-let* ((first (seq-find (lambda (e) (equal (cdddr e) focused)) blocks)))
      (setq blocks (cons first (delq first blocks))))
    (let ((entries (append blocks (nreverse tabs))))
      (mapcar (lambda (e)
                (cons (if (cdr (seq-filter (lambda (o) (equal (car o) (car e)))
                                           entries))
                          (format "%s <%s>" (car e) (cadr e))
                        (car e))
                      (cddr e)))
              entries))))

(defvar rexel-read-choice-function #'rexel--read-choice
  "Function that reads one of a session's blocks and tabs.
Its arguments are the prompt, an alist of (NAME . CHOICE) with the
default first, and the session's connection, ID and view.
It returns the CHOICE read: (block . BLOCK-ID) or (tab . TAB).")

(defun rexel--read-choice (prompt choices &rest _)
  "Return the CHOICE read with PROMPT from CHOICES, an alist kept in order."
  (let ((table (lambda (string pred action)
                 (if (eq action 'metadata)
                     '(metadata (display-sort-function . identity)
                                (cycle-sort-function . identity))
                   (complete-with-action action choices string pred)))))
    (cdr (assoc (completing-read prompt table nil t nil nil (caar choices))
                choices))))

(defun rexel--with-processes (conn sid bids callback)
  "Call CALLBACK with the process reports of the blocks BIDS.
The blocks are of session SID on CONN.  The reports are the answers of
`terminal.process' as (BLOCK-ID . REPORT); a block whose report takes
more than 1.5 seconds is left out."
  (let* ((pending (length bids))
         (reports nil)
         (timer nil)
         (finish (lambda ()
                   (when timer
                     (cancel-timer timer)
                     (setq timer nil)
                     (funcall callback reports)))))
    (setq timer (run-at-time 1.5 nil finish))
    (if (null bids)
        (funcall finish)
      (dolist (bid bids)
        (rexel--request
         conn "com.superlogical.terminal.process"
         `((session_id . ,sid) (block_id . ,bid))
         (lambda (result err)
           (unless err
             (push (cons bid result) reports))
           (when (zerop (cl-decf pending))
             (funcall finish))))))))

(defun rexel--read-attach-choice (conn sid view callback)
  "Ask for one of the blocks or tabs of session SID's VIEW on CONN.
Call CALLBACK with the CHOICE of `rexel--attach-choices' read.  The blocks'
process reports, which name them, are fetched first.  A block without one
is named by its label.  Nothing is asked, or done with the answer, once
the session is detached."
  (let ((attached (lambda () (assoc sid (rexel--conn-sessions conn)))))
    (rexel--with-processes
     conn sid (mapcar #'car (rexel--view-terminal-blocks view))
     (lambda (processes)
       (when (funcall attached)
         (rexel--prompt-later
          (lambda ()
            (let ((choice (funcall rexel-read-choice-function
                                   "Rex block or layout: "
                                   (rexel--attach-choices view processes)
                                   conn sid view)))
              (when (and choice (funcall attached))
                (funcall callback choice))))))))))

(defun rexel--reattach (conn)
  "Attach CONN again to its sessions and resume their typing and sizing.
A block whose `data.connect' went with the connection is streamed again."
  (setf (rexel--conn-attaching conn) (mapcar #'car (rexel--conn-sessions conn)))
  (pcase-dolist (`(,sid . ,_) (rexel--conn-sessions conn))
    (dolist (buf (rexel--session-buffers sid))
      (with-current-buffer buf
        (when (eq rexel--conn conn)
          ;; The server may have run the write whose answer was lost, so
          ;; it is not sent again.
          (when rexel--out-inflight
            (message "rexel: %s: keys typed as the connection dropped may be lost"
                     (buffer-name)))
          (setq rexel--out-inflight nil
                rexel--size-inflight nil
                rexel--theme-inflight nil)
          (unless rexel--data-state
            (setq rexel--data-state 'closed)))))
    (rexel--request
     conn "session.attach" `((session_id . ,sid))
     (lambda (result err)
       (setf (rexel--conn-attaching conn)
             (delete sid (rexel--conn-attaching conn)))
       (cond
        ;; Detached while the request was in flight; the detach may have
        ;; reached the server first.
        ((not (assoc sid (rexel--conn-sessions conn)))
         (unless err
           (rexel--request conn "session.detach" `((session_id . ,sid)))))
        ((equal (alist-get 'code err) "session_not_found")
         (rexel--forget-session conn sid))
        (err (message "rexel: session.attach: %s" (rexel--error-string err)))
        (t
         (dolist (buf (rexel--session-buffers sid))
           (rexel--flush-out buf))
         (rexel--apply-view conn sid (alist-get 'view result))
         (dolist (buf (rexel--session-buffers sid))
           (with-current-buffer buf
             (rexel--sync-theme)
             (rexel--sync-size)))))))))

(defun rexel--view-focused-block (view)
  "Return the ID of VIEW's focused terminal block, else of its first one."
  (let ((blocks (rexel--view-terminal-blocks view))
        (bid (alist-get 'focused_block_id (rexel--active-tab view))))
    (if (assoc bid blocks) bid (caar blocks))))

(defun rexel--show-focused-block (conn view)
  "Pop to the buffer of VIEW's focused block on CONN.
Without a focused terminal block, pop to VIEW's first terminal block."
  (rexel--pop-to-block conn (rexel--view-focused-block view)))

(defun rexel--active-tab (view)
  "Return VIEW's active tab, or nil."
  (seq-find (lambda (w) (equal (alist-get 'window_id w)
                               (alist-get 'active_window_id view)))
            (alist-get 'windows view)))

(defun rexel--schedule-view-refresh (conn sid)
  "Refresh session SID's view on CONN shortly, coalescing bursts of events."
  (unless (assoc sid (rexel--conn-refresh-timers conn))
    (push (cons sid (run-at-time 0.1 nil #'rexel--refresh-view conn sid))
          (rexel--conn-refresh-timers conn))))

(defun rexel--refresh-view (conn sid)
  "Fetch session SID's view on CONN and reconcile block buffers."
  (setf (alist-get sid (rexel--conn-refresh-timers conn) nil t #'equal) nil)
  (when (rexel--ready-p conn)
    (rexel--request conn "session.view" `((session_id . ,sid))
                    (lambda (view err)
                      (cond
                       ;; Destroyed while the request was in flight; the
                       ;; session_destroyed event cleans up.
                       ((equal (alist-get 'code err) "session_not_found"))
                       (err (message "rexel: session.view: %s"
                                     (rexel--error-string err)))
                       ;; The session may have been detached while the
                       ;; request was in flight.
                       ((assoc sid (rexel--conn-sessions conn))
                        (rexel--apply-view conn sid view)))))))

(defun rexel--forget-session (conn sid)
  "Drop session SID from CONN and kill its block buffers."
  (when-let* ((timer (alist-get sid (rexel--conn-refresh-timers conn)
                                nil nil #'equal)))
    (cancel-timer timer))
  (setf (alist-get sid (rexel--conn-refresh-timers conn) nil t #'equal) nil
        (alist-get sid (rexel--conn-sessions conn) nil t #'equal) nil
        (alist-get sid (rexel--conn-revisions conn) nil t #'equal) nil
        (rexel--conn-attaching conn) (delete sid (rexel--conn-attaching conn)))
  (mapc #'rexel--discard-buffer (rexel--session-buffers sid)))

(defun rexel--destroy-if-empty (conn sid)
  "Destroy session SID on CONN if it is attached and holds no block.
A session with a block buffer on CONN holds one, and the server's
`session.list_blocks' also has the blocks outside of every tab.
A session with a request of `rexel--busy-request' unanswered stays."
  (cl-flet ((unused-p ()
              (and (assoc sid (rexel--conn-sessions conn))
                   (not (member sid (rexel--conn-attaching conn)))
                   (not (seq-some
                         (lambda (buf)
                           (eq (buffer-local-value 'rexel--conn buf) conn))
                         (rexel--session-buffers sid))))))
    (when (unused-p)
      (rexel--session-request
       conn sid "session.list_blocks"
       (lambda (result)
         (when (and (seq-empty-p (alist-get 'blocks result))
                    (unused-p)
                    (not (member sid (rexel--conn-busy conn))))
           (rexel--session-request conn sid "session.destroy" #'ignore)))))))

(defun rexel--session-buffers (sid)
  "Return the block buffers of session SID on any connection, streaming or not."
  (and sid
       (seq-filter (lambda (buf)
                     (equal (buffer-local-value 'rexel--session-id buf) sid))
                   (buffer-list))))

(defun rexel--view-terminal-blocks (view)
  "Return the (BLOCK-ID . LABEL) of every terminal block in VIEW."
  (mapcan (lambda (tab)
            (mapcar (lambda (entry)
                      (cons (alist-get 'block_id (car entry))
                            (alist-get 'label (car entry))))
                    (rexel--tab-terminal-blocks tab)))
          (alist-get 'windows view)))

(defun rexel--apply-view (conn sid view)
  "Make CONN's block buffers for session SID match VIEW.
Responses can arrive out of order, so a VIEW older than the one
already applied is ignored."
  (let ((revision (alist-get 'revision view))
        (applied (alist-get sid (rexel--conn-revisions conn) nil nil #'equal)))
    (unless (and applied revision (< revision applied))
      (let ((names (rexel--view-block-names view)))
        (setf (alist-get sid (rexel--conn-revisions conn) nil nil #'equal)
              revision)
        (setf (alist-get sid (rexel--conn-sessions conn) nil nil #'equal)
              (alist-get 'label view))
        (pcase-dolist (`(,bid . ,_) names)
          (unless (member bid (rexel--conn-closing conn))
            (rexel--ensure-block-buffer conn sid bid)))
        (dolist (buf (rexel--session-buffers sid))
          (with-current-buffer buf
            (if (assoc rexel--block-id names)
                (progn
                  (setq rexel--owner (rexel--view-block-owner
                                      view rexel--block-id))
                  (force-mode-line-update))
              (rexel--discard-buffer buf))))
        ;; Tabs that moved up trade names: the first pass frees the
        ;; names the second one gives.
        (dotimes (_ 2)
          (pcase-dolist (`(,bid . ,name) names)
            (when-let* ((buf (gethash bid (rexel--conn-blocks conn)))
                        ((buffer-live-p buf))
                        (name (rexel--buffer-name conn sid name)))
              (with-current-buffer buf
                ;; A name another buffer keeps is taken as NAME<2>.
                (unless (equal (buffer-name)
                               (generate-new-buffer-name name (buffer-name)))
                  (rename-buffer name t))))))))))

(defun rexel--annotation ()
  "Return the state of this block's stream unless it is live.
`ghostel-annotate-buffer' shows it in buffer pickers."
  (pcase rexel--data-state
    ('live nil)
    ('closed "disconnected")
    (_ "connecting")))

(defun rexel--buffer-name (conn sid name)
  "Return the buffer name for the block called NAME in session SID on CONN.
NAME is the block's name in its tab, see `rexel--view-block-names', or
the end of its ID."
  (format "*rex:%s%s:%s*"
          (or (alist-get sid (rexel--conn-sessions conn) nil nil #'equal) "?")
          (if (rexel--conn-host conn) (concat "@" (rexel--conn-host conn)) "")
          name))

(defun rexel--stale-block-buffer (bid)
  "Return a block buffer for BID whose stream ended, on any connection."
  (seq-find (lambda (buf)
              (and (equal (buffer-local-value 'rexel--block-id buf) bid)
                   (eq (buffer-local-value 'rexel--data-state buf) 'closed)))
            (buffer-list)))

(defun rexel--ensure-block-buffer (conn sid bid)
  "Return the buffer for block BID of session SID on CONN, creating it.
A buffer whose stream ended earlier is reset and streamed again.  A new
buffer is named after the end of BID until a view names its tab."
  (let ((buf (gethash bid (rexel--conn-blocks conn))))
    (unless (buffer-live-p buf)
      (setq buf (rexel--stale-block-buffer bid)))
    (if (and buf (not (eq (buffer-local-value 'rexel--data-state buf) 'closed)))
        buf
      (rexel--attach-block-buffer
       conn sid bid
       (or buf (rexel--buffer-name conn sid (substring bid -6)))))))

(defun rexel--attach-block-buffer (conn sid bid buffer-or-name)
  "Make a terminal for block BID of session SID on CONN, return its buffer.
BUFFER-OR-NAME is as for `ghostel-make-terminal'."
  (with-current-buffer (ghostel-make-terminal buffer-or-name
                                              :size '(80 . 24)
                                              :send #'rexel--pty-out
                                              :resize #'rexel--resize
                                              :send-files #'rexel--send-files)
    (puthash bid (current-buffer) (rexel--conn-blocks conn))
    ;; A block streamed again on its connection keeps its typed bytes
    ;; and the requests it has in flight.
    (unless (eq rexel--conn conn)
      (setq rexel--out-pending nil
            rexel--out-inflight nil
            rexel--size-inflight nil
            rexel--theme-inflight nil))
    (setq rexel--conn conn
          rexel--session-id sid
          rexel--block-id bid
          rexel--shell-pid nil
          rexel--shell-program nil
          rexel--data-state nil
          rexel--requested-size nil
          rexel--size-take nil
          rexel--close-on-kill 'unasked)
    (setq-local ghostel-annotation-function #'rexel--annotation)
    ;; A directory the shell reports itself is put under the prefix
    ;; `default-directory' has.
    (when-let* ((prefix (rexel--conn-prefix conn))
                ((not (string-prefix-p prefix default-directory))))
      (setq default-directory (concat prefix "/")))
    (rexel-block-mode 1)
    (add-hook 'window-buffer-change-functions #'rexel--window-shown nil t)
    (add-hook 'window-selection-change-functions #'rexel--window-selected nil t)
    (add-function :after after-focus-change-function #'rexel--focus-changed)
    (add-hook 'kill-buffer-query-functions #'rexel--kill-buffer-query nil t)
    (add-hook 'kill-buffer-hook #'rexel--block-buffer-killed nil t)
    (rexel--open-data-connection conn (current-buffer))
    (current-buffer)))

(defun rexel--discard-buffer (buf)
  "Kill the block buffer BUF, if live, and leave its block to the server."
  (when (buffer-live-p buf)
    (let ((rexel--keep-blocks t))
      (kill-buffer buf))))

(defun rexel--closable-p ()
  "Return non-nil when killing this buffer may close its block.
The server refuses to while the session is being reattached."
  (and (not rexel--keep-blocks) rexel--block-id (rexel--ready-p rexel--conn)
       (not (member rexel--session-id (rexel--conn-attaching rexel--conn)))))

(defun rexel--kill-buffer-query ()
  "Decide whether killing this buffer closes its block.
`rexel-kill-buffer-action' says what is asked.  Return nil to keep the
buffer, as `kill-buffer-query-functions' expects."
  (let* ((closable (rexel--closable-p))
         (action rexel-kill-buffer-action)
         (keep-buffer
          (and closable (eq action 'close) (ghostel-command-running-p)
               (not (yes-or-no-p (format "Rex block %s runs a command; close it? "
                                         (buffer-name)))))))
    (setq rexel--close-on-kill
          (cond ((or keep-buffer (not closable)) 'unasked)
                ((eq action 'ask)
                 (y-or-n-p (format "Close Rex block %s on the server too? "
                                   (buffer-name))))
                (t (eq action 'close))))
    (when (and (listp rexel--kill-declined)
               (or keep-buffer (and (eq action 'ask) (not rexel--close-on-kill))))
      (push rexel--session-id rexel--kill-declined))
    (not keep-buffer)))

(defun rexel--close-block (conn sid bid)
  "Close block BID of session SID on CONN, ending its program.
Until the answer no view makes a buffer for the block, and after it none
older than the close does."
  (push bid (rexel--conn-closing conn))
  (cl-flet ((answered (result err)
              (setf (rexel--conn-closing conn)
                    (delete bid (rexel--conn-closing conn)))
              (when err
                (message "rexel: block.close: %s" err))
              (when-let* ((revision (alist-get 'revision result))
                          ((assoc sid (rexel--conn-sessions conn))))
                (cl-callf (lambda (applied) (max revision (or applied 0)))
                    (alist-get sid (rexel--conn-revisions conn)
                               nil nil #'equal)))))
    (condition-case err
        (rexel--request
         conn "block.close" `((session_id . ,sid) (block_id . ,bid))
         (lambda (result err)
           (answered result (and err (rexel--error-string err)))))
      (error (answered nil (error-message-string err))))))

(defun rexel--block-buffer-killed ()
  "Close the connections of the block buffer being killed.
Close its block too if `rexel--kill-buffer-query' decided so; a kill that
ran no query follows `rexel-kill-buffer-action', `ask' keeping the block."
  (when (if (eq rexel--close-on-kill 'unasked)
            (eq rexel-kill-buffer-action 'close)
          rexel--close-on-kill)
    (cond ((rexel--closable-p)
           (rexel--close-block rexel--conn rexel--session-id rexel--block-id))
          ((and rexel--conn (not rexel--keep-blocks))
           (message "rexel: %s: not connected, its block keeps running"
                    (buffer-name)))))
  (when rexel--conn
    (remhash rexel--block-id (rexel--conn-blocks rexel--conn)))
  (when (or rexel--uploads rexel--upload-queue)
    (rexel--upload-fail (current-buffer) "buffer killed"))
  (rexel--close-stream))

(defun rexel--window-selected (window)
  "Focus the block on the server when WINDOW showing it is selected.
This also makes the block's tab the active one in other Rex clients.
With the keyboard focus, or when Emacs cannot tell as on a terminal that
does not report it, take the block and size it too."
  (when (and (eq window (selected-window)) (rexel--ready-p rexel--conn))
    (rexel--request rexel--conn "session.focus_block"
                    `((session_id . ,rexel--session-id)
                      (block_id . ,rexel--block-id))
                    #'ignore)
    (when (ghostel-buffer-focused-p (current-buffer))
      (rexel--sync-size window))))

(defvar rexel--focus-timer nil
  "The timer of `rexel--focus-changed', while it waits to run.")

(defun rexel--focus-changed ()
  "Take the focused frames' selected blocks once the focus has settled.
This runs as `after-focus-change-function', also while Emacs reads a key."
  (unless rexel--focus-timer
    (setq rexel--focus-timer (run-at-time 0 nil #'rexel--take-focused))))

(defun rexel--take-focused ()
  "Take and size the block in the selected window of each frame with focus.
A frame whose focus Emacs cannot tell reports no focus change."
  (setq rexel--focus-timer nil)
  (dolist (frame (frame-list))
    (when (eq (frame-focus-state frame) t)
      (let ((window (frame-selected-window frame)))
        (with-current-buffer (window-buffer window)
          (when rexel--block-id
            (rexel--sync-size window)))))))

(defun rexel--pty-out (data)
  "Type DATA, bytes from ghostel's key encoder, into this block.
The server runs block methods concurrently, so writes are sent one
at a time and bytes typed meanwhile are coalesced into the next.
Typing while a write is unanswered probes the block's connections."
  (if (null rexel--conn)
      (message "rexel: not connected; use M-x rexel-attach")
    (setq rexel--out-pending (concat rexel--out-pending data))
    (when (eq rexel--out-inflight rexel--conn)
      (rexel--probe (rexel--conn-ws rexel--conn))
      (when-let* ((ws (and rexel--stream (rexel--stream-ws rexel--stream))))
        (rexel--probe ws)))
    (rexel--flush-out (current-buffer))))

(defun rexel--flush-out (buf)
  "Send BUF's pending typed bytes unless a write is in flight."
  (with-current-buffer buf
    (when (and (rexel--ready-p rexel--conn)
               (not (eq rexel--out-inflight rexel--conn))
               (not (member rexel--session-id (rexel--conn-attaching rexel--conn)))
               (> (length rexel--out-pending) 0))
      (let ((data rexel--out-pending)
            (conn rexel--conn))
        (setq rexel--out-pending nil
              rexel--out-inflight conn)
        (condition-case err
            (rexel--request
             conn "com.superlogical.terminal.write"
             `((session_id . ,rexel--session-id)
               (block_id . ,rexel--block-id)
               (args . ((data . ,(decode-coding-string data 'utf-8-unix)))))
             (lambda (_result err)
               (when err
                 (message "rexel: terminal.write: %s" (rexel--error-string err)))
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (setq rexel--out-inflight nil)
                   (rexel--flush-out buf)))))
          (error
           (setq rexel--out-inflight nil)
           (message "rexel: dropped %d typed bytes: %s"
                    (length data) (error-message-string err))))))))

(defun rexel--on-block-event (conn payload)
  "Handle the block event PAYLOAD of a session on CONN.
A terminal's `process_changed' event names the foreground process, in
place of ghostel's PTY probe: a foreground other than the shell means a
command is running, and the shell's cwd becomes `default-directory': as
it is on this machine's server, and below the TRAMP prefix of another
server that has one."
  (when-let* ((buf (gethash (alist-get 'block_id payload)
                            (rexel--conn-blocks conn)))
              ((buffer-live-p buf))
              ((equal (alist-get 'creator_name payload) rexel--terminal-creator))
              ((equal (alist-get 'name payload) "process_changed")))
    (let* ((event (alist-get 'payload payload))
           (fg (alist-get 'foreground event))
           (shell (rexel--shell-process fg (alist-get 'ancestors event))))
      (with-current-buffer buf
        ;; Rex leaves out the ancestors when there are too many or one
        ;; exited, so FG without them counts as the shell only if none is
        ;; known yet or it has the known shell's pid.
        (when (and shell (or (not (eq shell fg)) (null rexel--shell-pid)
                             (eql (alist-get 'pid fg) rexel--shell-pid)))
          (rexel--note-shell shell))
        ;; The cwd is a path on the server's machine.
        (when-let* ((cwd (alist-get 'cwd shell)))
          (if (null (rexel--conn-host conn))
              (ghostel-set-directory cwd)
            (when-let* ((prefix (rexel--conn-prefix conn)))
              (ghostel-set-directory (concat prefix cwd)))))
        ;; OSC 133 marks from the shell are exact and take over once seen.
        (unless (let ((prompt (ghostel-last-prompt)))
                  (and prompt (not (eq prompt rexel--stale-prompt))))
          (ghostel-set-command-running
           (and fg rexel--shell-pid
                (not (eql (alist-get 'pid fg) rexel--shell-pid)))))))))

(defun rexel--note-shell (shell)
  "Remember SHELL, this block's shell process as Rex reports it.
A shell that became another program, as after `exec', may send no OSC 133
marks, so those seen so far stop counting.  Rex names a process by its
path and argv0; an `exec' that keeps both goes unnoticed."
  (let ((pid (alist-get 'pid shell))
        (program (list (alist-get 'path shell) (alist-get 'argv0 shell))))
    (when (and (eql pid rexel--shell-pid)
               (not (equal program rexel--shell-program)))
      (setq rexel--stale-prompt (ghostel-last-prompt)))
    (setq rexel--shell-pid pid
          rexel--shell-program program)))

(defun rexel--shell-process (fg ancestors)
  "Return the shell among the foreground process FG and its ANCESTORS.
ANCESTORS run from FG's parent to the terminal's program.  The shell is the
process below the `login' wrapper, or FG when only the wrapper is above it.
Return nil while the wrapper itself is in the foreground; before it execs
`login' the wrapper is still called `rex'."
  (let ((chain (append ancestors nil)))
    (when (equal (alist-get 'name (car (last chain))) "login")
      (setq chain (butlast chain)))
    (cond (chain (car (last chain)))
          ((not (member (alist-get 'name fg) '("login" "rex"))) fg))))


;;; Data connection

(defvar-local rexel--hello-refused nil
  "Non-nil when the server refused a HELLO since the last WELCOME.")

(defun rexel--open-data-connection (conn buf)
  "Stream the terminal of BUF's block from CONN into its ghostel terminal."
  (let ((sid (buffer-local-value 'rexel--session-id buf))
        (bid (buffer-local-value 'rexel--block-id buf)))
    (rexel--request
     conn "data.connect"
     `((session_id . ,sid) (block_id . ,bid)
       (data_ref_id . "terminal") (subprotocol . "rex.data.terminal.v1"))
     (lambda (result err)
       (cond
        (err (rexel--data-failed buf (format "data.connect: %s"
                                             (rexel--error-string err))))
        ((buffer-live-p buf)
         (with-current-buffer buf
           (setq rexel--data-state 'welcome
                 rexel--stream (rexel--make-stream))
           (condition-case err
               (rexel--open-stream (rexel--hello (alist-get 'ticket result)))
             (error (rexel--data-failed buf (error-message-string err)))))))))))

(defun rexel--hello (ticket)
  "Return the HELLO message opening a data connection with TICKET."
  (concat "\x01" (base64-decode-string ticket t)))

(defun rexel--open-data-ws (protocol hello on-message on-close)
  "Open a data connection speaking PROTOCOL for this block and send HELLO.
ON-MESSAGE and ON-CLOSE are `websocket-open' callbacks.  Return the
WebSocket."
  (rexel--ws-open (rexel--conn-endpoint rexel--conn) "/api/data" protocol
                  (lambda (ws)
                    (websocket-send ws (make-websocket-frame
                                        :opcode 'binary :payload hello :completep t)))
                  on-message on-close))

(defun rexel--handshake (bytes)
  "Classify the data handshake answer BYTES.
Return `welcome' for a WELCOME, a REJECT's reason code, or nil."
  (cond ((and (>= (length bytes) 32) (= (aref bytes 0) #x81)) 'welcome)
        ((and (>= (length bytes) 2) (= (aref bytes 0) #x82)) (aref bytes 1))))

(defun rexel--open-stream (hello)
  "Connect this buffer's stream to the data endpoint and send HELLO."
  (let ((buf (current-buffer))
        (stream rexel--stream))
    (setf (rexel--stream-ws stream)
          (rexel--open-data-ws
           "rex.data.terminal.v1" hello
           (lambda (_ws frame)
             (rexel--on-data buf stream (websocket-frame-payload frame)))
           (lambda (_ws) (rexel--on-data-close buf stream))))))

(defun rexel--resume-stream ()
  "Reopen this buffer's dropped data connection and continue its stream.
The server continues from the first byte not yet received."
  (let ((s rexel--stream))
    (setf (rexel--stream-resume s) rexel--data-state)
    (setq rexel--data-state 'welcome)
    (condition-case nil
        (rexel--open-stream
         (concat "\x02" (rexel--stream-token s)
                 (bindat-pack (bindat-type uintr 64) (rexel--stream-received s))))
      (error (rexel--on-data-close (current-buffer) s)))))

(defun rexel--close-stream ()
  "Mark this buffer's stream closed and close its WebSocket.
The stream is forgotten before the close, so that
`rexel--on-data-close' ignores it."
  (setq rexel--data-state 'closed)
  (when-let* ((stream rexel--stream))
    (setq rexel--stream nil)
    (ignore-errors (websocket-close (rexel--stream-ws stream)))))

(defun rexel--data-failed (buf reason)
  "Close BUF's stream and report REASON."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (rexel--close-stream)
      (message "rexel: %s: %s" (buffer-name) reason))))

(defun rexel--end-stream (buf reason)
  "End BUF's stream, noting REASON in the terminal."
  (when (and (buffer-live-p buf)
             (not (eq (buffer-local-value 'rexel--data-state buf) 'closed)))
    (with-current-buffer buf
      (rexel--close-stream)
      (ghostel-write-output (format "\r\n[rexel: %s]\r\n" reason)))))

(defun rexel--restart-stream (buf reason)
  "End BUF's stream for REASON and have its block streamed afresh.
While the control connection is up and the block still exists, the next
view refresh streams the block into BUF again."
  (with-current-buffer buf
    (rexel--end-stream buf reason)
    (when (and (rexel--ready-p rexel--conn)
               (eq (gethash rexel--block-id (rexel--conn-blocks rexel--conn)) buf))
      (remhash rexel--block-id (rexel--conn-blocks rexel--conn))
      (rexel--schedule-view-refresh rexel--conn rexel--session-id))))

(defun rexel--on-data-close (buf stream)
  "Note that BUF's data connection STREAM closed.
The stream is resumed if the connection dropped after its WELCOME,
and restarted if it dropped before."
  (when (and (buffer-live-p buf)
             (eq stream (buffer-local-value 'rexel--stream buf)))
    (with-current-buffer buf
      (if (memq rexel--data-state '(snapshot live))
          (rexel--resume-stream)
        (rexel--restart-stream buf "stream closed")))))

(defun rexel--on-data (buf stream bytes)
  "Handle BYTES, one binary message of BUF's data connection STREAM."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (pcase (and (eq stream rexel--stream) rexel--data-state)
        ('welcome (rexel--on-welcome bytes))
        ((and (or 'snapshot 'live) state)
         (cl-incf (rexel--stream-received stream) (length bytes))
         (if (eq state 'snapshot)
             (rexel--on-snapshot-bytes bytes)
           (rexel--on-live-bytes bytes)))))))

(defun rexel--on-welcome (bytes)
  "Read the data handshake answer BYTES.
A refused RESUME streams the block afresh.  A refused HELLO uses up its
ticket, and is tried once more with a new one."
  (pcase (rexel--handshake bytes)
    ('welcome
     (let ((s rexel--stream))
       (setq rexel--hello-refused nil)
       (setf (rexel--stream-token s) (substring bytes 1 17))
       (if-let* ((state (rexel--stream-resume s)))
           (progn
             (setq rexel--data-state state)
             ;; Sizes asked for during the RESUME were not sent.
             (when (eq state 'live)
               (rexel--sync-theme)
               (rexel--sync-size)))
         (setf (rexel--stream-marker s)
               (format "\e_rex1;%d;"
                       (bindat-unpack (bindat-type uintr 64) bytes 24)))
         (setq rexel--data-state 'snapshot))
       (when (> (length bytes) 32)
         (rexel--on-data (current-buffer) s (substring bytes 32)))))
    ((and (pred integerp) reason)
     (cond
      ((rexel--stream-resume rexel--stream)
       (rexel--restart-stream (current-buffer)
                              (format "resume refused, reason %d" reason)))
      (rexel--hello-refused
       (rexel--data-failed (current-buffer)
                           (format "data connection rejected, reason %d" reason)))
      (t
       (setq rexel--hello-refused t)
       (rexel--restart-stream (current-buffer)
                              (format "data connection rejected, reason %d" reason)))))
    (_ (rexel--data-failed (current-buffer) "malformed data handshake"))))

(defconst rexel--max-record-bytes (ash 1 28)
  "Snapshot records claiming more bytes than this end the stream.")

(defun rexel--snapshot-walk (bytes pos)
  "Walk the snapshot records in BYTES from the record header at POS.
A header is 10 bytes: a u16 tag and a u32 payload length, then a CRC.
Return (t END) when the FINISH record ends at END.
Otherwise return (nil END NEED): END is the start of the first
incomplete record and NEED the bytes from END that complete it,
or its header if that is cut off.
Signal an error on a record longer than `rexel--max-record-bytes'."
  (let ((len (length bytes)))
    (catch 'done
      (while t
        (when (> (+ pos 10) len)
          (throw 'done (list nil pos 10)))
        (let* ((payload-len (logior (aref bytes (+ pos 2))
                                    (ash (aref bytes (+ pos 3)) 8)
                                    (ash (aref bytes (+ pos 4)) 16)
                                    (ash (aref bytes (+ pos 5)) 24)))
               (next (+ pos 10 payload-len)))
          (when (> payload-len rexel--max-record-bytes)
            (error "Snapshot record of %d bytes" payload-len))
          (when (> next len)
            (throw 'done (list nil pos (- next pos))))
          (when (= 6 (logior (aref bytes pos) (ash (aref bytes (1+ pos)) 8)))
            (throw 'done (list t next)))
          (setq pos next))))))

(defun rexel--on-snapshot-bytes (bytes)
  "Collect snapshot BYTES and restore the terminal once it is complete."
  (let ((s rexel--stream))
    (push bytes (rexel--stream-tail s))
    (when (>= (apply #'+ (mapcar #'length (rexel--stream-tail s)))
              (rexel--stream-need s))
      (let* ((data (apply #'concat (nreverse (rexel--stream-tail s))))
             (first (null (rexel--stream-snapshot s)))
             (walk (if (and first (not (string-prefix-p "GHOSTSNP" data)))
                       "stream is not a terminal snapshot"
                     (condition-case err
                         (rexel--snapshot-walk data (if first 10 0))
                       (error (error-message-string err))))))
        (if (stringp walk)
            (rexel--end-stream (current-buffer) walk)
          (pcase-let ((`(,finished ,end ,need) walk))
            (when (> end 0)
              (push (substring data 0 end) (rexel--stream-snapshot s)))
            (setf (rexel--stream-tail s) (list (substring data end))
                  (rexel--stream-need s) need)
            (when finished
              (rexel--restore-snapshot))))))))

(defun rexel--restore-snapshot ()
  "Restore the collected snapshot and go live with the bytes after it."
  (let* ((s rexel--stream)
         (snapshot (apply #'concat (nreverse (rexel--stream-snapshot s))))
         (rest (car (rexel--stream-tail s))))
    (setf (rexel--stream-snapshot s) nil
          (rexel--stream-tail s) nil)
    (condition-case err
        (ghostel-restore-snapshot snapshot)
      (error (rexel--end-stream (current-buffer)
                                (format "bad snapshot: %s"
                                        (error-message-string err))))
      (:success
       (setq rexel--data-state 'live)
       (when (> (length rest) 0)
         (rexel--on-live-bytes rest))
       (rexel--sync-theme)
       (rexel--sync-size)
       (rexel--fetch-status)))))

(defconst rexel--max-message-bytes 65536
  "Longest stream control message held back waiting for its end.")

(defun rexel--marker-prefix-length (data marker)
  "Return how many trailing bytes of DATA are a proper prefix of MARKER."
  (let ((k (min (1- (length marker)) (length data))))
    (while (and (> k 0) (not (string-suffix-p (substring marker 0 k) data)))
      (setq k (1- k)))
    k))

(defun rexel--split-stream (data marker)
  "Split DATA into terminal output and control messages.
MARKER introduces a control message, which ends at ST.  Return
\(ITEMS . PENDING): ITEMS are output strings and (message . JSON)
conses in order; PENDING is the tail that may begin a message and
must be prepended to the next chunk.  A message still unterminated
after `rexel--max-message-bytes' passes through as output."
  (let ((start 0)
        (len (length data))
        (pending "")
        items)
    (while (< start len)
      (let ((pos (string-search marker data start)))
        (if (null pos)
            (let ((keep (rexel--marker-prefix-length (substring data start) marker)))
              (when (< start (- len keep))
                (push (substring data start (- len keep)) items))
              (setq pending (substring data (- len keep))
                    start len))
          (when (< start pos)
            (push (substring data start pos) items))
          (let ((end (string-search "\e\\" data (+ pos (length marker)))))
            (if (null end)
                (progn
                  (if (> (- len pos) rexel--max-message-bytes)
                      (push (substring data pos) items)
                    (setq pending (substring data pos)))
                  (setq start len))
              (push (cons 'message (substring data (+ pos (length marker)) end))
                    items)
              (setq start (+ end 2)))))))
    (cons (nreverse items) pending)))

(defun rexel--on-live-bytes (bytes)
  "Feed stream BYTES to the terminal, applying embedded control messages."
  (pcase-let* ((s rexel--stream)
               (`(,items . ,pending)
                (rexel--split-stream (concat (rexel--stream-pending s) bytes)
                                     (rexel--stream-marker s))))
    (setf (rexel--stream-pending s) pending)
    (dolist (item items)
      (if (consp item)
          (rexel--handle-message (cdr item))
        ;; The server answers the program's terminal queries.
        (ghostel-write-output item t)))))

(defun rexel--handle-message (json)
  "Apply the stream control message JSON."
  (let ((msg (rexel--parse-json (decode-coding-string json 'utf-8))))
    (pcase (alist-get 'type msg)
      ("resize"
       (let ((cols (alist-get 'cols msg))
             (rows (alist-get 'rows msg)))
         (when (and (natnump cols) (natnump rows) (> cols 0) (> rows 0))
           (ghostel-set-size (cons cols rows))
           (force-mode-line-update))))
      ("child_exited"
       (message "rexel: %s: program exited with status %s"
                (buffer-name) (alist-get 'exit_code msg))))))


;;; Uploads

(defconst rexel--upload-chunk-bytes 4096
  "Bytes of a file sent in one message of an upload.
The server takes up to 65536.  A message that fits the send buffer of a
Unix socket, 8192 bytes on macOS, is written at once; for the rest of a
larger one Emacs looks for room only every 20 ms.")

(defconst rexel--upload-batch-max 16
  "Most uploads a terminal takes at a time.")

(defvar-local rexel--upload-reporter nil
  "Progress reporter of the batch in progress.")

(defvar rexel--upload-slice 0.05
  "Seconds an upload sends for before Emacs reads what other processes sent.")

(cl-defstruct (rexel--upload (:constructor rexel--make-upload) (:copier nil))
  "One file on its way to the machine a block runs on.
FILE is the local file, NAME the name it gets, SIZE its bytes, nil until
`rexel--upload-fetch', and SENT those sent so far.  REF names its data
stream and WS is that stream's WebSocket.  STATE is `begin' while the
ticket is requested, `welcome' until the server answers the HELLO, then
`ready', `sending', `ending' once the end marker is sent, and `done'.
TEMP is non-nil when FILE is a copy to delete afterwards."
  file name size ref ws state (sent 0) temp)

(defun rexel--uuid ()
  "Return a random version 4 UUID in lower case."
  (format "%08x-%04x-4%03x-%04x-%012x"
          (random (ash 1 32)) (random (ash 1 16)) (random (ash 1 12))
          (logior #x8000 (random (ash 1 14))) (random (ash 1 48))))

(defun rexel--send-files (files)
  "Hand FILES to the program in this block by typing their paths.
When the server runs on this machine and all FILES are local, their own
paths are typed.  Otherwise the files are uploaded in batches of
`rexel--upload-batch-max' and the server types the paths it stores them
under; calls made in one command share a batch."
  (cond
   ((null rexel--conn)
    (user-error "Not connected; use M-x rexel-attach"))
   ((and (null (rexel--conn-host rexel--conn))
         (not (seq-some #'file-remote-p files)))
    (ghostel-paste-string
     (concat (mapconcat #'shell-quote-argument files " ") " ")))
   (t
    (dolist (file files)
      (unless (and (file-regular-p file) (file-readable-p file))
        (user-error "Cannot upload %s: not a readable file" file)))
    (setq rexel--upload-queue (append rexel--upload-queue files))
    (run-at-time 0 nil #'rexel--upload-next (current-buffer)))))

(defun rexel--upload-next (buf)
  "Start the next batch of BUF's queued files once it can go out.
That is once no write is innermost and BUF's session is attached.  A
fetch of a remote file inside a write's wait would keep it from
returning, until it counts as stalled."
  (if (or (rexel--write-p (car rexel--writes))
          (and (buffer-live-p buf)
               (with-current-buffer buf
                 (and (rexel--ready-p rexel--conn)
                      (member rexel--session-id
                              (rexel--conn-attaching rexel--conn))))))
      (run-at-time 0.01 nil #'rexel--upload-next buf)
    (rexel--upload-start buf)))

(defun rexel--upload-start (buf)
  "Start the next batch of BUF's queued files unless one is in progress.
Remote files are fetched, then every file is announced before any is
sent, so the server refuses a batch over its limits with nothing sent."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (and rexel--upload-queue (null rexel--uploads))
        (let* ((files (seq-take rexel--upload-queue rexel--upload-batch-max))
               (count (length files))
               (batch (and (> count 1) (rexel--uuid)))
               (uploads (mapcar (lambda (file)
                                  (rexel--make-upload
                                   :file file :state 'begin
                                   :name (file-name-nondirectory file)
                                   :ref (concat "upload:" (rexel--uuid))))
                                files)))
          (setq rexel--upload-queue (nthcdr count rexel--upload-queue)
                rexel--uploads uploads)
          (condition-case err
              (progn
                (unless (rexel--ready-p rexel--conn)
                  (error "Not connected"))
                (when (seq-every-p (lambda (upload)
                                     (rexel--upload-fetch buf upload))
                                   uploads)
                  (setq rexel--upload-reporter
                        (make-progress-reporter
                         (format "Uploading %d file%s to %s..."
                                 count (if (= count 1) "" "s")
                                 (rexel--conn-name rexel--conn))
                         0 (max 1 (apply #'+ (mapcar #'rexel--upload-size
                                                     uploads)))))
                  (seq-do-indexed (lambda (upload index)
                                    (rexel--upload-begin buf upload batch index count))
                                  uploads)))
            (error (when (rexel--upload-current-p buf (car uploads))
                     (rexel--upload-fail buf (error-message-string err))))))))))

(defun rexel--upload-fetch (buf upload)
  "Make the file of UPLOAD local and note its size.
Return nil if the batch in BUF ended during the fetch, which can block
while timers and filters run."
  (let* ((file (rexel--upload-file upload))
         (copy (rexel--local-copy file)))
    (if (not (rexel--upload-current-p buf upload))
        (when copy
          (ignore-errors (delete-file copy)))
      (when copy
        (setf (rexel--upload-file upload) copy
              (rexel--upload-temp upload) t))
      (or (setf (rexel--upload-size upload)
                (file-attribute-size
                 (file-attributes
                  (file-truename (rexel--upload-file upload)))))
          (error "Cannot read %s" file)))))

(defun rexel--local-copy (file)
  "Return a temporary local copy of the remote FILE, or nil for a local FILE.
A compressed file is copied as it is."
  (let ((inhibit-file-name-handlers
         (cons 'jka-compr-handler inhibit-file-name-handlers))
        (inhibit-file-name-operation 'file-local-copy))
    (file-local-copy file)))

(defun rexel--upload-begin (buf upload batch index count)
  "Request the ticket for UPLOAD in BUF and connect its stream with it.
With BATCH, UPLOAD is file INDEX of COUNT in the batch of that ID."
  (rexel--request
   rexel--conn "com.superlogical.terminal.upload_begin"
   `((session_id . ,rexel--session-id)
     (block_id . ,rexel--block-id)
     (args . ((ref . ,(rexel--upload-ref upload))
              (name . ,(rexel--upload-name upload))
              (size . ,(rexel--upload-size upload))
              ,@(when batch
                  `((batch . ,batch) (index . ,index) (count . ,count))))))
   (lambda (result err)
     (when (rexel--upload-current-p buf upload)
       (if err
           (rexel--upload-fail buf (format "%s: %s" (rexel--upload-name upload)
                                           (rexel--error-string err)))
         (with-current-buffer buf
           (condition-case err
               (rexel--upload-open buf upload
                                   (rexel--hello (alist-get 'ticket result)))
             (error (rexel--upload-fail buf (error-message-string err))))))))))

(defun rexel--upload-open (buf upload hello)
  "Connect the stream of UPLOAD in BUF to the data endpoint and send HELLO."
  (let ((ws (rexel--open-data-ws
             "rex.data.bytes.v1" hello
             (lambda (_ws frame)
               (rexel--on-upload-message buf upload
                                         (websocket-frame-payload frame)))
             (lambda (ws) (rexel--on-upload-close buf upload ws)))))
    (set-process-filter (websocket-conn ws) #'rexel--upload-filter)
    (setf (rexel--upload-ws upload) ws
          (rexel--upload-state upload) 'welcome)))

(defun rexel--upload-current-p (buf upload)
  "Return non-nil if UPLOAD is part of the batch in progress in BUF."
  (and (buffer-live-p buf)
       (memq upload (buffer-local-value 'rexel--uploads buf))))

(defun rexel--upload-filter (process output)
  "Keep the OUTPUT of an upload connection's PROCESS and pass it on.
websocket.el drops the status of a close frame; `rexel--close-status'
reads it from the kept bytes, which are few: a handshake and pings."
  (process-put process :upload-in
               (concat (process-get process :upload-in) output))
  (rexel--ws-filter process output))

(defun rexel--close-status (bytes)
  "Return the status of the close frame in BYTES, or nil if they have none.
BYTES are all a server sent on a WebSocket, with or without its
handshake response.  A close frame without a status gives 1005.
Frames websocket.el cannot read end the search."
  (let ((pos (if (string-prefix-p "HTTP/" bytes)
                 (+ 4 (or (string-search "\r\n\r\n" bytes) (length bytes)))
               0))
        (total (length bytes))
        status)
    (ignore-error websocket-unparseable-frame
      (catch 'websocket-incomplete-frame
        (while (and (not status) (< pos total))
          (let* ((len (websocket-get-payload-len
                       (substring bytes (1+ pos) (min total (+ pos 10)))))
                 (start (+ pos 1 (cdr len)))
                 (end (+ start (car len))))
            (when (> end total)
              (throw 'websocket-incomplete-frame nil))
            (when (= 8 (logand 15 (aref bytes pos)))
              (setq status (if (< (car len) 2)
                               1005
                             (logior (ash (aref bytes start) 8)
                                     (aref bytes (1+ start))))))
            (setq pos end)))))
    status))

(defun rexel--on-upload-message (buf upload bytes)
  "Handle BYTES, the server's answer to the HELLO of UPLOAD in BUF."
  (when (rexel--upload-current-p buf upload)
    (pcase (rexel--handshake bytes)
      ((and 'welcome (guard (eq (rexel--upload-state upload) 'welcome)))
       (setf (rexel--upload-state upload) 'ready)
       (rexel--upload-pump buf))
      ((and (pred integerp) reason)
       (rexel--upload-fail buf (format "%s: data connection rejected, reason %d"
                                       (rexel--upload-name upload) reason)))
      (_ (rexel--upload-fail buf "malformed data handshake")))))

(defun rexel--upload-pump (buf)
  "Send the first unfinished file of BUF's batch once its stream is ready.
Files go one at a time, in the order their paths are typed.  With all of
them done, report it and start the next batch."
  (with-current-buffer buf
    (let ((upload (seq-find (lambda (u) (not (eq (rexel--upload-state u) 'done)))
                            rexel--uploads))
          (count (length rexel--uploads)))
      (cond
       ((null upload)
        (rexel--upload-end)
        (message "rexel: uploaded %d file%s to %s"
                 count (if (= count 1) "" "s") (rexel--conn-name rexel--conn))
        ;; This runs in a sentinel, and fetching a remote file can block.
        (run-at-time 0 nil #'rexel--upload-next buf))
       ((eq (rexel--upload-state upload) 'ready)
        (setf (rexel--upload-state upload) 'sending)
        (rexel--upload-send-chunk buf upload))))))

(defun rexel--upload-pause (process)
  "Return the seconds to wait before the next chunk goes to PROCESS.
Timers due at once run in a row and Emacs reads no process output between
them, so once per `rexel--upload-slice' the next chunk waits 1 ms."
  (let ((now (float-time)))
    (if (< (- now (or (process-get process :upload-paused) 0)) rexel--upload-slice)
        0
      (process-put process :upload-paused now)
      0.001)))

(defun rexel--upload-send-chunk (buf upload)
  "Send the next chunk of UPLOAD in BUF, and the rest from a timer.
A chunk waits while `rexel--writes-overdue' holds writes."
  (when (rexel--upload-current-p buf upload)
    (if rexel--writes-overdue
        (run-at-time 0.01 nil #'rexel--upload-send-chunk buf upload)
      (let (returned)
        (unwind-protect
            (progn
              (condition-case err
                  (rexel--upload-write buf upload)
                ;; A batch that failed meanwhile closed the connection.
                (error (when (rexel--upload-current-p buf upload)
                         (rexel--upload-fail
                          buf (format "%s: %s" (rexel--upload-name upload)
                                      (error-message-string err))))))
              (setq returned t))
          ;; A `throw' through the write would end the chain of chunks.
          (when (and (not returned) (rexel--upload-current-p buf upload))
            (rexel--upload-fail
             buf (format "%s: the write was interrupted"
                         (rexel--upload-name upload)))))))))

(defun rexel--upload-write (buf upload)
  "Write the next chunk of UPLOAD in BUF and schedule the one after it.
An empty message after the last chunk ends the file.  Emacs reads no keys
while a chunk waits for room in the socket, so a chunk that stalls (see
`rexel--watched-write') fails the batch."
  (let* ((process (websocket-conn (rexel--upload-ws upload)))
         (sent (rexel--upload-sent upload))
         (end (min (rexel--upload-size upload)
                   (+ sent rexel--upload-chunk-bytes)))
         (chunk (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (when (< sent end)
                    (insert-file-contents-literally
                     (rexel--upload-file upload) nil sent end))
                  (buffer-string))))
    (unless (= (length chunk) (- end sent))
      (error "File changed during the upload"))
    (rexel--watched-write
     process
     (lambda ()
       (when (rexel--upload-current-p buf upload)
         (rexel--upload-fail buf (format "%s: the connection stalled"
                                         (rexel--upload-name upload)))))
     (lambda ()
       (process-send-string process (rexel--binary-frame chunk)))
     (length chunk))
    ;; A ping would wait behind the file's data; a chunk the socket
    ;; took shows the connection alive.
    (process-put process :last-read (float-time))
    (if (= sent end)
        (setf (rexel--upload-state upload) 'ending)
      (setf (rexel--upload-sent upload) end)
      (with-current-buffer buf
        (when rexel--upload-reporter
          (progress-reporter-update
           rexel--upload-reporter
           (apply #'+ (mapcar #'rexel--upload-sent rexel--uploads)))))
      (run-at-time (rexel--upload-pause process) nil
                   #'rexel--upload-send-chunk buf upload))))

(defun rexel--binary-frame (payload)
  "Return a client's binary WebSocket frame holding the unibyte PAYLOAD.
PAYLOAD is shorter than 65536 bytes.  The mask key is zero, which leaves
the payload as it is; masking it in `websocket-encode-frame' would take
most of an upload's time."
  (let ((len (length payload)))
    (concat (if (< len 126)
                (unibyte-string #x82 (logior #x80 len))
              (unibyte-string #x82 #xfe (ash len -8) (logand len #xff)))
            "\0\0\0\0" payload)))

(defun rexel--on-upload-close (buf upload ws)
  "Handle the close of WS, the connection of UPLOAD in BUF.
The server closes with status 1000 once it has the file, and the last of
a batch after typing the paths.  Any other close fails the batch."
  (when (rexel--upload-current-p buf upload)
    (let ((status (rexel--close-status
                   (or (process-get (websocket-conn ws) :upload-in) ""))))
      (if (and (eql status 1000) (eq (rexel--upload-state upload) 'ending))
          (progn
            (setf (rexel--upload-state upload) 'done)
            (rexel--upload-pump buf))
        (rexel--upload-fail
         buf (format "%s: %s" (rexel--upload-name upload)
                     (if status
                         (format "server closed the upload with status %d" status)
                       "connection lost")))))))

(defun rexel--upload-end ()
  "Forget this buffer's batch, delete its temporary copies, return the batch."
  (let ((uploads rexel--uploads))
    (setq rexel--uploads nil
          rexel--upload-reporter nil)
    (dolist (upload uploads)
      (when (rexel--upload-temp upload)
        (ignore-errors (delete-file (rexel--upload-file upload)))))
    uploads))

(defun rexel--upload-fail (buf reason)
  "Give up BUF's batch and its queued files, reporting REASON.
A connection closed before the end of its file makes the server discard
the batch, so no path is typed."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      ;; Forgotten first, so that the closes below are ignored.
      (setq rexel--upload-queue nil)
      (let ((uploads (rexel--upload-end)))
        ;; A close frame would wait for room in a stalled connection.
        (dolist (upload uploads)
          (when-let* ((ws (rexel--upload-ws upload)))
            (ignore-errors (delete-process (websocket-conn ws)))))
        (message "rexel: %s: upload failed: %s" (buffer-name) reason)))))


;;; Sizing

(defvar rexel--shown-in-minibuffer nil
  "The (WINDOW . BUFFER) of blocks to take once the minibuffer exits.")

(defun rexel--in-minibuffer-p ()
  "Return non-nil while the selected window is an active minibuffer."
  (and (active-minibuffer-window)
       (eq (selected-window) (active-minibuffer-window))))

(defun rexel--resize (size)
  "Ask the server to give this block SIZE, if that changes anything.
SIZE is (COLS . ROWS), fitting the windows that show the block.  With
`rexel--size-take', also take ownership of the block from other
clients.  A new size takes it too while the block has the focus, as for
`ghostel-buffer-focused-p'.  The terminal resizes when the server
echoes the size."
  (when (and (rexel--ready-p rexel--conn) (eq rexel--data-state 'live)
             (or rexel--size-take (not (equal size rexel--requested-size))))
    (setq rexel--requested-size size
          rexel--size-take (or rexel--size-take
                               (ghostel-buffer-focused-p (current-buffer))))
    (rexel--send-size (current-buffer))))

(defun rexel--sync-size (&optional window)
  "Take this block and size it to the windows showing it, if any.
From a minibuffer a window may only preview the block, so that waits for
the minibuffer to exit and for WINDOW, by default any window showing the
block now, to show it still.  ghostel asks for no size in copy mode, so
there the block is taken and sized when the mode is left."
  (if (rexel--in-minibuffer-p)
      (progn
        (dolist (w (if window (list window) (get-buffer-window-list nil nil t)))
          (cl-pushnew (cons w (current-buffer)) rexel--shown-in-minibuffer
                      :test #'equal))
        (add-hook 'minibuffer-exit-hook #'rexel--minibuffer-exited))
    (when-let* ((shown (get-buffer-window (current-buffer) t)))
      (setq rexel--size-take t)
      (ghostel-fit-window shown))))

(defun rexel--window-shown (window)
  "Take the block and sync its size after WINDOW started showing it."
  (with-current-buffer (window-buffer window)
    (rexel--sync-size window)))

(defun rexel--minibuffer-exited ()
  "Sync the blocks of `rexel--shown-in-minibuffer' once the command is done."
  (remove-hook 'minibuffer-exit-hook #'rexel--minibuffer-exited)
  (run-at-time 0 nil #'rexel--sync-shown))

(defun rexel--sync-shown ()
  "Take and size the blocks of `rexel--shown-in-minibuffer' still shown there."
  (if (rexel--in-minibuffer-p)
      (add-hook 'minibuffer-exit-hook #'rexel--minibuffer-exited)
    (let ((buffers nil))
      (pcase-dolist (`(,window . ,buffer)
                     (prog1 rexel--shown-in-minibuffer
                       (setq rexel--shown-in-minibuffer nil)))
        (when (and (window-live-p window) (eq (window-buffer window) buffer))
          (cl-pushnew buffer buffers)))
      (dolist (buffer buffers)
        (with-current-buffer buffer
          (rexel--sync-size))))))

(defun rexel--send-size (buf)
  "Send BUF's requested size, taking the block first if asked to.
Resizes run concurrently on the server, so one is sent at a time and
the latest requested size follows when it completes.  The size applies
once this client owns the block, when the server reports it in the stream."
  (with-current-buffer buf
    (unless (or (eq rexel--size-inflight rexel--conn)
                (member rexel--session-id (rexel--conn-attaching rexel--conn)))
      (let* ((conn rexel--conn)
             (sid rexel--session-id)
             (bid rexel--block-id)
             (size rexel--requested-size)
             (take rexel--size-take)
             (done (lambda (_result err)
                     (when err
                       (message "rexel: resize: %s" (rexel--error-string err)))
                     (when (buffer-live-p buf)
                       (with-current-buffer buf
                         (setq rexel--size-inflight nil)
                         (when (and rexel--requested-size
                                    (or rexel--size-take
                                        (not (equal size rexel--requested-size))))
                           (rexel--send-size buf))))))
             (resize (lambda ()
                       (when (buffer-live-p buf)
                         (setq size (buffer-local-value 'rexel--requested-size buf)))
                       (if size
                           (rexel--request
                            conn "com.superlogical.terminal.resize"
                            `((session_id . ,sid) (block_id . ,bid)
                              (args . ((columns . ,(car size))
                                       (rows . ,(cdr size)))))
                            done)
                         (funcall done nil nil)))))
        (setq rexel--size-take nil
              rexel--size-inflight conn)
        (condition-case err
            (if (not take)
                (funcall resize)
              (rexel--request
               conn "session.own_block"
               `((session_id . ,sid) (block_ids . ,(vector bid)) (mode . "take"))
               (lambda (result err)
                 (if err
                     (funcall done result err)
                   (funcall resize)))))
          (error
           (setq rexel--size-inflight nil)
           (signal (car err) (cdr err))))))))


;;; Owner

(defun rexel--size-tag-take (event)
  "Take the block whose mode line the mouse EVENT clicked, and size it."
  (interactive "e")
  (rexel--window-shown (posn-window (event-start event))))

(defvar rexel--size-tag-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'rexel--size-tag-take)
    map)
  "Keymap of the mode line tag of a block that another client sizes.")

(defun rexel--size-tag ()
  "Return the mode line tag of a block that another client sizes, else nil.
It names the size that client gave the terminal."
  (when-let* (((eq rexel--data-state 'live))
              (rexel--owner)
              (id (rexel--conn-client-id rexel--conn))
              ((stringp id))
              ((not (equal rexel--owner id)))
              (size (ghostel-terminal-size)))
    (concat (propertize (format "[Sized by another client %dx%d]"
                                (car size) (cdr size))
                        'face 'warning
                        'help-echo "mouse-1: Take the block and size it to its windows here"
                        'mouse-face 'mode-line-highlight
                        'local-map rexel--size-tag-map)
            " ")))

(add-to-list 'mode-line-misc-info '(rexel-block-mode (:eval (rexel--size-tag))))


;;; Colors

(defun rexel--theme ()
  "Return the colors of ghostel's faces as arguments for `set_theme'."
  (let-alist (ghostel-theme-colors)
    `((foreground . ,.foreground)
      (background . ,.background)
      (scheme . ,(symbol-name .scheme))
      ,@(when .cursor
          `((cursor . ,.cursor)))
      ,@(when (> (length .palette) 0)
          `((palette
             . ,(vconcat
                 (seq-map-indexed (lambda (color i) (format "%d=%s" i color))
                                  .palette))))))))

(defun rexel--sync-theme ()
  "Send this block's colors to the server.
They apply while this client owns the block, and the server answers the
color queries of the block's programs with them.  One request is sent at
a time; colors that changed meanwhile follow when it completes."
  (when (and (rexel--ready-p rexel--conn)
             (eq rexel--data-state 'live)
             (not (eq rexel--theme-inflight rexel--conn))
             (not (member rexel--session-id (rexel--conn-attaching rexel--conn))))
    (let ((theme (rexel--theme))
          (conn rexel--conn)
          (buf (current-buffer)))
      (setq rexel--theme-inflight conn)
      (condition-case err
          (rexel--request
           conn "com.superlogical.terminal.set_theme"
           `((session_id . ,rexel--session-id) (block_id . ,rexel--block-id)
             (args . ,theme))
           (lambda (_result err)
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (when (eq rexel--theme-inflight conn)
                   (setq rexel--theme-inflight nil)
                   (cond
                    (err (message "rexel: set_theme: %s"
                                  (rexel--error-string err)))
                    ((not (equal theme (rexel--theme)))
                     (rexel--sync-theme))))))))
        ;; A theme change must not fail for a connection that is going down.
        (error
         (setq rexel--theme-inflight nil)
         (message "rexel: set_theme: %s" (error-message-string err)))))))

(defun rexel--sync-themes (&rest _)
  "Send the colors of every block buffer to its server."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (rexel--sync-theme))))

(add-hook 'enable-theme-functions #'rexel--sync-themes)
(add-hook 'disable-theme-functions #'rexel--sync-themes)


;;; Program status

;; A snapshot leaves out the records of OSC 7501 reports; the server
;; keeps them, and `terminal.program_status' lists them.

(defun rexel--fetch-status ()
  "Replace this block's program status records with the server's.
A block or session gone meanwhile, or a server without the method, is no error."
  (when (and (rexel--ready-p rexel--conn)
             (eq rexel--data-state 'live)
             (not (member rexel--session-id
                          (rexel--conn-attaching rexel--conn))))
    (let ((buf (current-buffer)))
      (condition-case err
          (rexel--request
           rexel--conn "com.superlogical.terminal.program_status"
           `((session_id . ,rexel--session-id) (block_id . ,rexel--block-id))
           (lambda (result err)
             (cond ((member (alist-get 'code err)
                            '("not_found" "method_not_found" "session_not_found")))
                   (err (message "rexel: program_status: %s"
                                 (rexel--error-string err)))
                   ((buffer-live-p buf)
                    (with-current-buffer buf
                      (rexel--apply-status (alist-get 'records result)))))))
        (error
         (message "rexel: program_status: %s" (error-message-string err)))))))

(defun rexel--status-entry (record app)
  "Return RECORD of `terminal.program_status' as the (ID . PLIST) ghostel takes.
APP is the app to record: the record's own, or the one the server resolved."
  (let-alist record
    (cons (or .id "")
          (list :state (intern-soft .state) :kind (intern-soft .kind)
                :progress .progress :app app :title .title :msg .msg))))

(defun rexel--apply-status (records)
  "Make RECORDS, as `terminal.program_status' lists them, this block's.
They show as the program reported them, without notifying."
  (ghostel-set-program-status
   (mapcar (lambda (record) (rexel--status-entry record (alist-get 'app record)))
           ;; Most recently updated first: RFC 3339 times with the server's
           ;; offset sort as strings.
           (seq-sort-by (lambda (record) (alist-get 'updated_at record))
                        #'string> records))
   t))


;;; Commands

(defun rexel--pick-session (conn callback)
  "Let the user pick a session on CONN and call CALLBACK with its ID."
  (rexel--request
   conn "session.list" nil
   (lambda (result err)
     (if err
         (message "rexel: session.list: %s" (rexel--error-string err))
       (let ((sessions (mapcar (lambda (s) (cons (alist-get 'label s)
                                                 (alist-get 'session_id s)))
                               (alist-get 'sessions result))))
         (if sessions
             (rexel--prompt-later
              (lambda () (funcall callback (rexel--choose "Rex session: " sessions))))
           (message "rexel: the server has no sessions")))))))

(defun rexel--choose (prompt alist)
  "Return the value of ALIST's only entry, or of one read with PROMPT."
  (if (cdr alist)
      (cdr (assoc (completing-read prompt alist nil t) alist))
    (cdar alist)))

;;;###autoload
(defun rexel-start-server ()
  "Start this machine's Rex server, which keeps running after Emacs exits."
  (interactive)
  (let ((rex (or (rexel--program)
                 (user-error "Cannot find %s; see `rexel-program'" rexel-program))))
    (unless (zerop (call-process rex nil nil nil "server" "start"))
      (user-error "Could not start the Rex server"))
    (message "Rex server running")))

(defun rexel--autostart-server ()
  "Start this machine's Rex server, as `rexel-autostart-server' allows."
  (when (and rexel-autostart-server
             (rexel--program)
             (or (eq rexel-autostart-server t)
                 (y-or-n-p "No Rex server is running.  Start one? ")))
    (rexel-start-server)))

(defun rexel--read-host ()
  "Return the Rex host to use, asking when more than one is known.
With no host known, offer to start this machine's server."
  (let* ((hosts (or (rexel--hosts)
                    (progn (rexel--autostart-server) (rexel--hosts))))
         (names (mapcar (lambda (h) (cons (or (car h) "this machine") h)) hosts)))
    (unless hosts
      (user-error "No Rex server found; see `rexel-hosts'"))
    (rexel--choose "Rex host: " names)))

(defun rexel--prefix-host (prefix)
  "Return the name of the host in the TRAMP PREFIX, without a port."
  (when-let* ((host (file-remote-p prefix 'host)))
    (replace-regexp-in-string "#.*\\'" "" host)))

(defun rexel--prefix-label (prefix)
  "Return the label of the server `rexel-tramp-hosts' gives PREFIX, or nil.
PREFIX is as `file-remote-p' returns it, and the option's prefixes are
compared in that form."
  (car (seq-find (lambda (entry)
                   (equal prefix (or (file-remote-p (cdr entry)) (cdr entry))))
                 rexel-tramp-hosts)))

(defun rexel--tramp-host (prefix hosts)
  "Return the one of HOSTS whose machine the TRAMP PREFIX reaches, or nil.
HOSTS are as `rexel--hosts' returns them.  It is the host that
`rexel-tramp-hosts' gives PREFIX, else one labelled with the host name
in PREFIX, else one with that name in an address."
  (let ((name (rexel--prefix-host prefix))
        (hosts (seq-filter (lambda (host) (stringp (car host))) hosts)))
    (or (when-let* ((label (rexel--prefix-label prefix)))
          (assoc-string label hosts t))
        (and name
             (or (assoc-string name hosts t)
                 (seq-find
                  (lambda (host)
                    (seq-some
                     (lambda (endpoint)
                       (and (rexel--network-endpoint-p endpoint)
                            (string= (downcase name)
                                     (downcase (url-host (url-generic-parse-url
                                                          endpoint))))))
                     (cdr host)))
                  hosts))))))

(defun rexel--conn-prefix (conn)
  "Return the TRAMP prefix of the files on CONN's server, or nil.
It is the one `rexel-tramp-hosts' gives the server, else that of the
TRAMP directory the server was last used for."
  (and (rexel--conn-host conn)
       (or (cdr (assoc-string (rexel--conn-host conn) rexel-tramp-hosts t))
           (rexel--conn-tramp conn))))

(defun rexel--connect-directory (callback &optional running)
  "Call CALLBACK with a ready connection to the server of `default-directory'.
That is this machine's server, also for a TRAMP directory on this
machine, and for one elsewhere the server `rexel--tramp-host' finds.
Without a server, offer to start this machine's and signal an error for
another machine, or with RUNNING do nothing."
  (let* ((prefix (file-remote-p default-directory))
         (prefix (and prefix
                      (not (ghostel-local-host-p (rexel--prefix-host prefix)))
                      prefix))
         (conn (seq-find (lambda (conn)
                           (and (rexel--ready-p conn)
                                (if prefix
                                    (equal prefix (rexel--conn-tramp conn))
                                  (null (rexel--conn-host conn)))))
                         rexel--conns))
         (host (lambda ()
                 (let ((hosts (rexel--hosts)))
                   (if prefix
                       (rexel--tramp-host prefix hosts)
                     (assq nil hosts)))))
         (use (lambda (conn)
                (when prefix
                  (setf (rexel--conn-tramp conn) prefix))
                (funcall callback conn))))
    (cond (conn (funcall use conn))
          (running
           ;; A server that cannot be dialled is one that does not run.
           (when-let* ((host (funcall host)))
             (condition-case nil (rexel--connect host use) (error nil))))
          (t (rexel--connect
              (or (funcall host)
                  (if prefix
                      (user-error "No Rex server known for %s; see `rexel-tramp-hosts'"
                                  prefix)
                    (rexel--autostart-server)
                    (funcall host))
                  (user-error "No Rex server runs on this machine"))
              use)))))

(defun rexel--find-session (conn label callback)
  "Call CALLBACK with the ID of the session labelled LABEL on CONN, or nil.
An attached session comes first.  Of several others, ask which."
  (if-let* ((attached (rassoc label (rexel--conn-sessions conn))))
      (funcall callback (car attached))
    (rexel--request
     conn "session.list" nil
     (lambda (result err)
       (if err
           (message "rexel: session.list: %s" (rexel--error-string err))
         (let ((ids (delq nil (seq-map
                               (lambda (s)
                                 (and (equal (alist-get 'label s) label)
                                      (alist-get 'session_id s)))
                               (alist-get 'sessions result)))))
           (if (cdr ids)
               (rexel--prompt-later
                (lambda ()
                  (when-let* ((sid (rexel--choose
                                    "Rex session: "
                                    (mapcar (lambda (id)
                                              (cons (format "%s <%s>" label
                                                            (substring id -6))
                                                    id))
                                            ids))))
                    (funcall callback sid))))
             (funcall callback (car ids)))))))))

(defun rexel--create-session (conn label cwd callback &optional tab-label)
  "Create a session labelled LABEL on CONN and call CALLBACK with its ID.
The session has one tab, labelled TAB-LABEL, with a shell in CWD.  A nil
or empty label is left to the server."
  (rexel--request
   conn "session.create"
   `(,@(unless (member label '(nil "")) `((label . ,label)))
     (initial_windows
      . ,(vector `(,@(unless (member tab-label '(nil ""))
                       `((window_label . ,tab-label)))
                   (layout . ,(rexel--new-shell-layout cwd))))))
   (lambda (result err)
     (if err
         (message "rexel: session.create: %s" (rexel--error-string err))
       (funcall callback (alist-get 'session_id result))))))

(defun rexel--process-running-p (report)
  "Return non-nil if REPORT, a `terminal.process' answer, shows a command.
A block runs one when its foreground process is not its shell."
  (let* ((fg (alist-get 'foreground report))
         (shell (rexel--shell-process fg (alist-get 'ancestors report))))
    (and shell (not (eql (alist-get 'pid fg) (alist-get 'pid shell))))))

(defun rexel--destroy-session (conn sid label &optional confirm)
  "Destroy session SID on CONN, labelled LABEL, ending its programs.
Ask first when blocks of it run a command or do not report their
processes, or with CONFIRM.  The server takes requests about a session
only from a connection attached to it, so CONN attaches for the time."
  (let ((attached (assoc sid (rexel--conn-sessions conn))))
    (cl-labels
        ((request (method callback)
           (rexel--session-request conn sid method callback))
         (finish (destroy)
           (cond (destroy (request "session.destroy" #'ignore))
                 ((not attached) (request "session.detach" #'ignore))))
         (ask (running)
           (let ((yes nil))
             (unwind-protect
                 (setq yes (yes-or-no-p
                            (if (zerop running)
                                (format "Destroy Rex session %s? " label)
                              (format "Rex session %s runs %d command%s; destroy it? "
                                      label running (if (= running 1) "" "s")))))
               (finish yes))))
         (decide (view)
           ;; A block being closed may report nothing any more.
           (let ((bids (seq-difference
                        (mapcar #'car (rexel--view-terminal-blocks view))
                        (rexel--conn-closing conn))))
             (rexel--with-processes
              conn sid bids
              (lambda (reports)
                (let ((running (- (length bids)
                                  (seq-count (lambda (report)
                                               (not (rexel--process-running-p
                                                     (cdr report))))
                                             reports))))
                  (if (or confirm (> running 0))
                      (rexel--prompt-later (lambda () (ask running)))
                    (finish t))))))))
      (cond ((member sid (rexel--conn-attaching conn))
             (message "rexel: the session is still being reattached"))
            (attached (request "session.view" #'decide))
            (t (request "session.attach"
                        (lambda (result) (decide (alist-get 'view result)))))))))

(defun rexel--ensure-block-buffer-context ()
  "Signal an error unless this buffer shows a block on a ready connection."
  (unless (and (rexel--ready-p rexel--conn) rexel--block-id)
    (user-error "Not in a connected Rex block buffer")))

(defconst rexel--shell-flavor "com.superlogical.terminal.shell"
  "Flavor of a new shell block.")

(defun rexel--new-shell-layout (&optional cwd)
  "Return a layout object holding one shell block started in CWD.
Without CWD the shell starts in the home directory."
  `((block . ((flavor . ,rexel--shell-flavor)
              ,@(when cwd `((options . ((cwd . ,cwd)))))))))

(defun rexel--shell-directory (conn)
  "Return `default-directory' as a directory on CONN's server, or nil.
A local directory is one on this machine's server.  A TRAMP directory is
one on the server `rexel--connect-directory' last used for its prefix,
unless it is relative to a home directory."
  (let ((prefix (file-remote-p default-directory)))
    (cond ((null prefix)
           (and (null (rexel--conn-host conn))
                (directory-file-name (expand-file-name default-directory))))
          ((if (rexel--conn-host conn)
               (equal prefix (rexel--conn-tramp conn))
             (ghostel-local-host-p (rexel--prefix-host prefix)))
           (let ((dir (file-local-name default-directory)))
             (and (file-name-absolute-p dir)
                  (not (string-prefix-p "~" dir))
                  (directory-file-name dir)))))))

(defun rexel--block-directory (callback)
  "Call CALLBACK with the directory for a shell beside the current block.
That is the working directory the server reports for the block's shell,
else `rexel--shell-directory', else nil."
  (when (member rexel--session-id (rexel--conn-attaching rexel--conn))
    (user-error "The session is still being reattached"))
  (let ((fallback (rexel--shell-directory rexel--conn)))
    (rexel--request
     rexel--conn "com.superlogical.terminal.process"
     `((session_id . ,rexel--session-id) (block_id . ,rexel--block-id))
     (lambda (result err)
       (when (and err (not (equal (alist-get 'code err) "conflict")))
         (message "rexel: terminal.process: %s" (rexel--error-string err)))
       (funcall callback
                (or (alist-get 'cwd (rexel--shell-process
                                     (alist-get 'foreground result)
                                     (alist-get 'ancestors result)))
                    fallback))))))

;;;###autoload
(defun rexel-attach (&optional ask)
  "Attach to a Rex session and show its terminals as ghostel buffers.
`rexel-attach-display' says what is shown; with prefix argument ASK,
ask for a block or a tab to show."
  (interactive "P")
  (ghostel-ensure-module)
  (rexel--connect
   (rexel--read-host)
   (lambda (conn)
     (rexel--pick-session
      conn (lambda (sid)
             (rexel--attach-session conn sid (if ask 'ask rexel-attach-display)))))))

;;;###autoload
(defun rexel-new-session (&optional label)
  "Create a Rex session with one shell, labelled LABEL, and attach to it.
`rexel-session-label-function' may change LABEL; a nil or empty label lets the
server choose one.  Interactively, ask for LABEL only with a prefix argument."
  (interactive
   (list (and current-prefix-arg (read-string "Session label: "))))
  (ghostel-ensure-module)
  (when rexel-session-label-function
    (setq label (funcall rexel-session-label-function (or label ""))))
  (let ((dir default-directory))
    (rexel--connect
     (rexel--read-host)
     (lambda (conn)
       (let ((default-directory dir))
         (rexel--create-session
          conn label (rexel--shell-directory conn)
          (lambda (sid) (rexel--attach-session conn sid))))))))

(defun rexel--recent-block (conn sid)
  "Return the ID of the block of session SID on CONN that was selected last.
Nil when no window has shown a block of the session."
  (seq-some (lambda (buf)
              (and (eq (buffer-local-value 'rexel--conn buf) conn)
                   (equal (buffer-local-value 'rexel--session-id buf) sid)
                   (buffer-local-value 'buffer-display-time buf)
                   (buffer-local-value 'rexel--block-id buf)))
            (buffer-list)))

(defun rexel--show-session (conn sid arg cwd &optional tab-label)
  "Show session SID on CONN as ARG, the prefix argument of `rexel', says.
Without ARG show the block selected last, else the focused one; a number
shows the block of that tab.  Any other ARG, or the lack of such a block,
opens a tab labelled TAB-LABEL with a shell in CWD."
  (rexel--with-session
   conn sid
   (lambda (view)
     (let* ((tabs (alist-get 'windows view))
            (bid (cond ((integerp arg)
                        (and (<= 1 arg (length tabs))
                             (rexel--tab-block (elt tabs (1- arg)))))
                       (arg nil)
                       ((rexel--recent-block conn sid))
                       (t (rexel--view-focused-block view)))))
       (unless (and bid (rexel--pop-to-block conn bid rexel--same-window))
         (rexel--open-tab conn sid cwd tab-label rexel--same-window))))))

(defun rexel--open-session (label &optional arg tab-label)
  "Show the session labelled LABEL, creating it.
It is on the server of `default-directory', see
`rexel--connect-directory'.  ARG is the prefix argument of `rexel'.  A
new shell starts in `default-directory', and a new tab is labelled
TAB-LABEL."
  (ghostel-ensure-module)
  (let ((dir default-directory))
    (rexel--connect-directory
     (lambda (conn)
       (let ((cwd (let ((default-directory dir))
                    (rexel--shell-directory conn))))
         (rexel--find-session
          conn label
          (lambda (sid)
            (if sid
                (rexel--show-session conn sid arg cwd tab-label)
              (rexel--create-session
               conn label cwd
               (lambda (sid) (rexel--show-session conn sid nil cwd))
               tab-label)))))))))

(defun rexel--default-session (&optional arg)
  "Return the label of the session `rexel' shows with prefix argument ARG."
  (or (and (not (equal arg '(16)))
           rexel-default-session-function
           (funcall rexel-default-session-function))
      rexel-default-session))

;;;###autoload
(defun rexel (&optional arg)
  "Show the Rex session labelled `rexel-default-session', creating it.
`rexel-default-session-function' can name another session for the
current buffer.  The session is on this machine's server, or in a TRAMP
directory on the server of its machine, see `rexel-tramp-hosts'.  Show
the block of it that was selected last, or after attaching to it the
focused one.
With a non-numeric prefix ARG, open a new tab with a shell in
`default-directory'; with two \\[universal-argument], in the session
labelled `rexel-default-session' whatever the function names.  With a
numeric prefix ARG, show the tab of that number, or open a new tab when
there is none.
A session without a terminal gets a new tab."
  (interactive "P")
  (rexel--open-session (rexel--default-session arg) arg))

(defun rexel-rename-session (label)
  "Label the current block's session LABEL.
A nil or empty LABEL restores the label the server chose."
  (interactive
   (progn
     (rexel--ensure-block-buffer-context)
     (list (read-string "Session label: "
                        (alist-get rexel--session-id
                                   (rexel--conn-sessions rexel--conn)
                                   nil nil #'equal)))))
  (rexel--ensure-block-buffer-context)
  (rexel--request rexel--conn "session.set_label"
                  `((session_id . ,rexel--session-id) (label . ,(or label "")))))

(defun rexel-detach ()
  "Detach from the current buffer's session and kill its block buffers."
  (interactive)
  (unless rexel--conn
    (user-error "Not in a Rex block buffer"))
  (when (rexel--ready-p rexel--conn)
    (rexel--request rexel--conn "session.detach"
                    `((session_id . ,rexel--session-id))))
  (rexel--forget-session rexel--conn rexel--session-id))

(defun rexel--show-new-blocks (conn sid result err method &optional action)
  "Show the first block METHOD created on CONN in session SID.
RESULT and ERR are the response, and ACTION the `display-buffer' action
to show the block with."
  (if err
      (message "rexel: %s: %s" method (rexel--error-string err))
    (when-let* ((bid (seq-first (alist-get 'block_ids result))))
      (pop-to-buffer (rexel--ensure-block-buffer conn sid bid) action))))

(defun rexel--open-tab (conn sid cwd &optional label action)
  "Open a tab with one shell in CWD in session SID on CONN and show it.
LABEL labels the tab unless nil or empty.  ACTION is the `display-buffer'
action to show the shell with."
  (rexel--busy-request
   conn sid "session.new_window"
   `((session_id . ,sid) (layout . ,(rexel--new-shell-layout cwd))
     ,@(unless (member label '(nil "")) `((window_label . ,label))))
   (lambda (result err)
     (rexel--show-new-blocks conn sid result err "session.new_window"
                             action))))

(defun rexel-split (&optional direction)
  "Split the current block with a new shell on side DIRECTION.
DIRECTION is `right', `below', `left' or `above'.  Without it, split
below if `window-splittable-p' allows, else right, and let
`pop-to-buffer' place the new block."
  (interactive)
  (rexel--ensure-block-buffer-context)
  (let* ((conn rexel--conn)
         (sid rexel--session-id)
         (bid rexel--block-id)
         (window (selected-window))
         (side (or direction
                   (if (window-splittable-p window) 'below 'right))))
    (rexel--block-directory
     (lambda (dir)
       (rexel--request
        conn "session.new_split"
        `((session_id . ,sid) (anchor_id . ,bid)
          (direction . ,(if (memq side '(right left)) "horizontal" "vertical"))
          (side . ,(if (memq side '(right below)) "after" "before"))
          (layout . ,(rexel--new-shell-layout dir)) (focus . t))
        (lambda (result err)
          (rexel--show-new-blocks
           conn sid result err "session.new_split"
           (when direction
             ;; A nil `window-height' keeps the half split instead of
             ;; fitting the still empty buffer.
             `(display-buffer-in-direction (window-height . nil)
                                           (direction . ,direction)
                                           (window . ,window))))))))))

(defun rexel-split-right ()
  "Split the current block with a new shell to its right."
  (interactive)
  (rexel-split 'right))

(defun rexel-split-below ()
  "Split the current block with a new shell below it."
  (interactive)
  (rexel-split 'below))

(defun rexel-split-left ()
  "Split the current block with a new shell to its left."
  (interactive)
  (rexel-split 'left))

(defun rexel-split-above ()
  "Split the current block with a new shell above it."
  (interactive)
  (rexel-split 'above))

(defun rexel-new-tab (label)
  "Open a tab labelled LABEL with one shell in the current session.
A tab is what the Rex API calls a window."
  (interactive "sTab label: ")
  (rexel--ensure-block-buffer-context)
  (let ((conn rexel--conn)
        (sid rexel--session-id))
    (rexel--block-directory
     (lambda (dir) (rexel--open-tab conn sid dir label)))))

(defun rexel--read-session ()
  "Return (CONN . SID) of the current block, else of a chosen attached session."
  (if (and (rexel--ready-p rexel--conn) rexel--session-id)
      (cons rexel--conn rexel--session-id)
    (let (sessions)
      (dolist (conn rexel--conns)
        (when (rexel--ready-p conn)
          (pcase-dolist (`(,sid . ,label) (rexel--conn-sessions conn))
            (push (cons (if (rexel--conn-host conn)
                            (format "%s@%s" label (rexel--conn-host conn))
                          label)
                        (cons conn sid))
                  sessions))))
      (unless sessions
        (user-error "No attached Rex session"))
      (rexel--choose "Rex session: " sessions))))

(defun rexel--read-tab (conn sid view)
  "Let the user pick one of the tabs of session SID's VIEW on CONN; return it."
  (cdr (funcall rexel-read-choice-function "Rex tab: "
                (seq-map-indexed
                 (lambda (tab i)
                   (cons (format "%d: %s" (1+ i) (alist-get 'label tab))
                         (cons 'tab tab)))
                 (alist-get 'windows view))
                conn sid view)))

(defun rexel--show-layout (conn sid layout blocks window)
  "Show LAYOUT's blocks of session SID on CONN in WINDOW, splitting it.
BLOCKS are the terminal blocks of the view; a block not among them leaves
its window as it is.  A split that does not fit shows its before side."
  (let ((split (alist-get 'split layout)))
    (if (null split)
        (when-let* ((bid (alist-get 'block_id layout))
                    ((assoc bid blocks)))
          (set-window-buffer window (rexel--ensure-block-buffer conn sid bid)))
      (let* ((horizontal (equal (alist-get 'direction split) "horizontal"))
             (total (if horizontal
                        (window-total-width window)
                      (window-total-height window)))
             (new (ignore-errors
                    (split-window window
                                  (round (* (alist-get 'ratio split) total))
                                  (if horizontal 'right 'below)))))
        (rexel--show-layout conn sid (alist-get 'before split) blocks window)
        (when new
          (rexel--show-layout conn sid (alist-get 'after split) blocks new))))))

;;;###autoload
(defun rexel-sync-layout (&optional choose)
  "Rebuild the selected frame's windows from a Rex tab's layout.
The tab is the one holding the current block or, outside a block buffer,
the active tab of a chosen attached session.  With prefix argument
CHOOSE, pick the tab from a list.
Every block shown becomes Emacs-owned and sized to its window.  Blocks
floating above the layout are left out; a tab without a layout shows
its focused block alone."
  (interactive "P")
  (pcase-let ((`(,conn . ,sid) (rexel--read-session))
              (bid rexel--block-id))
    (rexel--with-view
     conn sid
     (lambda (view)
       (if choose
           (rexel--prompt-later
            (lambda ()
              (rexel--tab-again
               conn sid (alist-get 'window_id (rexel--read-tab conn sid view))
               (apply-partially #'rexel--layout-windows conn sid))))
         (rexel--layout-windows
          conn sid view
          (or (rexel--block-tab view bid) (rexel--active-tab view))))))))

(defun rexel--tab-windows (tab)
  "Return the layout to show for TAB: its own, else its focused block alone.
Nil when TAB holds no terminal."
  (or (rexel--tab-layout tab)
      (when-let* ((bid (rexel--tab-block tab)))
        `((block_id . ,bid)))))

(defun rexel--layout-windows (conn sid view tab)
  "Rebuild the selected frame's windows from TAB of session SID's VIEW on CONN.
Select the window of the tab's focused block.  A tab whose layout is
empty shows its focused block alone."
  (let ((layout (rexel--tab-windows tab))
        (window (seq-find (lambda (w) (not (window-parameter w 'window-side)))
                          (window-list nil 'no-mini))))
    (if (null layout)
        (message "rexel: Rex tab %s holds no terminal" (rexel--tab-name view tab))
      (delete-other-windows window)
      (set-window-dedicated-p window nil)
      (rexel--show-layout conn sid layout (rexel--view-terminal-blocks view) window)
      (when-let* ((buf (gethash (alist-get 'focused_block_id tab)
                                (rexel--conn-blocks conn)))
                  (w (get-buffer-window buf)))
        (select-window w)))))

(defun rexel-close-block ()
  "Close the current block on the server, ending its program."
  (interactive)
  (rexel--ensure-block-buffer-context)
  (when (yes-or-no-p (format "Close Rex block %s? " (buffer-name)))
    (rexel--close-block rexel--conn rexel--session-id rexel--block-id)))

(defun rexel-upload-file (files &optional buffer)
  "Send FILES to the program in the Rex block of BUFFER, by default this one.
The files are uploaded to the block's machine and their paths typed into
the terminal.  On this machine's server, local files are not uploaded.
Interactively, read a file in a block buffer; in a Dired buffer, send the
marked files to a block buffer read from the minibuffer."
  (interactive
   (if (derived-mode-p 'dired-mode)
       (list (dired-get-marked-files)
             (read-buffer "Upload to Rex block: " nil t
                          (lambda (b)
                            (buffer-local-value
                             'rexel--block-id
                             (if (consp b) (cdr b) (get-buffer b))))))
     (rexel--ensure-block-buffer-context)
     (list (list (read-file-name "Upload file: " nil nil t)))))
  (with-current-buffer (or buffer (current-buffer))
    (rexel--ensure-block-buffer-context)
    (rexel--send-files (mapcar #'expand-file-name files))))

(defun rexel-cancel-upload ()
  "Cancel the files being uploaded to the current block."
  (interactive)
  (unless (or rexel--uploads rexel--upload-queue)
    (user-error "No upload in progress"))
  (rexel--upload-fail (current-buffer) "cancelled"))


;;; Block mode

(defvar rexel-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "3" #'rexel-split-right)
    (define-key map "2" #'rexel-split-below)
    (define-key map [right] #'rexel-split-right)
    (define-key map [down] #'rexel-split-below)
    (define-key map [left] #'rexel-split-left)
    (define-key map [up] #'rexel-split-above)
    (define-key map (kbd "RET") #'rexel-split)
    (define-key map "c" #'rexel-new-tab)
    (define-key map "a" #'rexel-attach)
    (define-key map "n" #'rexel-new-session)
    (define-key map "=" #'rexel-sync-layout)
    (define-key map "$" #'rexel-rename-session)
    (define-key map "u" #'rexel-upload-file)
    (define-key map "x" #'rexel-close-block)
    (define-key map "d" #'rexel-detach)
    map)
  "Commands available behind `rexel-prefix-key' in block buffers.")

(defvar rexel-block-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (rexel--key rexel-prefix-key) rexel-prefix-map)
    map)
  "Keymap of `rexel-block-mode'; binds `rexel-prefix-key'.")

(define-minor-mode rexel-block-mode
  "Minor mode of ghostel buffers showing a Rex block."
  :lighter " Rex"
  :keymap rexel-block-mode-map)

(provide 'rexel)
;;; rexel.el ends here
