;;; rexel.el --- Rex multiplexer client for ghostel -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Daniel Kraus <daniel@kraus.my>

;; Author: Daniel Kraus <daniel@kraus.my>
;; URL: https://github.com/dakra/rexel
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ghostel "0.60.0") (websocket "1.15"))
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
;; `rexel-attach' picks a session.  In a block buffer `rexel-prefix-key'
;; (C-t) prefixes the layout commands: `%' and `"' split, `c' opens a
;; window, `x' closes the block, `d' detaches.
;;
;; Both connections are WebSockets on a server's Unix socket.  Servers
;; come from `rexel-program' and `rexel-hosts'; a remote server is
;; reached through an ssh forward of its socket.

;;; Code:

(require 'bindat)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'url-util)
(require 'ghostel)
(require 'websocket)

(declare-function ghostel--restore-snapshot "ghostel-module" (term data))

(defgroup rexel nil
  "Rex multiplexer client for ghostel."
  :group 'ghostel
  :prefix "rexel-")

(defcustom rexel-program "rex"
  "The rex command-line program, used to find servers."
  :type 'string)

(defcustom rexel-hosts nil
  "Rex servers to offer besides those `rexel-program' knows.
Each entry is (LABEL . SOCKET), the Unix socket path of the server, for
a remote server the local end of an ssh forward of its socket.  An entry
replaces a host of the same label known to Rex."
  :type '(alist :key-type string :value-type file))

(defcustom rexel-prefix-key "C-t"
  "Prefix key for `rexel-prefix-map' in block buffers."
  :type 'key-sequence)

(defconst rexel--api-revision 8
  "Control API revision this client speaks.")

(defconst rexel--terminal-creator "com.superlogical.terminal"
  "Creator name of Rex terminal blocks.")

(defconst rexel--shell-flavor "com.superlogical.terminal.shell"
  "Flavor of a new shell block.")


;;; Server connection

(cl-defstruct (rexel--conn (:constructor rexel--make-conn))
  "One control connection to a Rex server.
HOST is the server's label, nil for this machine's server."
  ws socket host client-id
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

(defun rexel--rex-json (&rest args)
  "Return the parsed JSON output of `rexel-program' run with ARGS, or nil."
  (when-let* ((rex (executable-find rexel-program)))
    (with-temp-buffer
      (when (zerop (apply #'call-process rex nil t nil
                          "--autostart=false" "--timeout" "2s" args))
        (ignore-errors (rexel--parse-json (buffer-string)))))))

(defun rexel--unix-endpoint (endpoints)
  "Return the socket path of the first unix:// endpoint in ENDPOINTS, or nil."
  (seq-some (lambda (e)
              (and (stringp e) (string-prefix-p "unix://" e)
                   (url-unhex-string (substring e (length "unix://")))))
            endpoints))

(defun rexel--hosts ()
  "Return the servers to attach to as (LABEL . SOCKET) pairs.
This machine's server has label nil and is listed when its socket exists.
Hosts without a Unix socket endpoint are left out."
  (let ((local (or (rexel--unix-endpoint
                    (list (alist-get 'endpoint
                                     (rexel--rex-json "server" "status" "--json"))))
                   (if (eq system-type 'darwin)
                       (expand-file-name "~/Library/Application Support/rex/server.sock")
                     "/run/rex/server.sock")))
        (hosts (mapcar (lambda (h)
                         (cons (alist-get 'label h)
                               (rexel--unix-endpoint (alist-get 'endpoints h))))
                       (alist-get 'hosts (rexel--rex-json "hosts" "list" "--json")))))
    (append (and (file-exists-p local) (list (cons nil local)))
            rexel-hosts
            (seq-remove (lambda (h) (or (null (cdr h))
                                        (assoc-string (car h) rexel-hosts t)))
                        hosts))))

(defun rexel--parse-json (string)
  "Parse JSON STRING into alists with symbol keys; null and false are nil."
  (json-parse-string string :object-type 'alist
                     :null-object nil :false-object nil))

(defun rexel--ws-open (path url protocol on-open on-message on-close)
  "Open a WebSocket to URL over the Unix socket PATH asking for PROTOCOL.
ON-OPEN, ON-MESSAGE and ON-CLOSE are `websocket-open' callbacks.
`websocket-open' only dials TCP, so the connection is made here and
handed to websocket.el for the handshake and framing."
  (let* ((key (websocket-genkey))
         (protocols (list protocol))
         (coding-system-for-read 'binary)
         (coding-system-for-write 'binary)
         (conn (make-network-process :name (format "rexel %s" url) :buffer nil
                                     :family 'local :service path
                                     :coding 'binary))
         (ws (websocket-inner-create
              :conn conn :url url :protocols protocols
              :on-open on-open :on-message on-message :on-close on-close
              :on-error #'rexel--ws-error
              :accept-string (websocket-calculate-accept key))))
    (process-put conn :websocket ws)
    (set-process-filter conn #'rexel--ws-filter)
    (set-process-sentinel conn (websocket-sentinel url conn key protocols
                                                   nil nil nil))
    (set-process-query-on-exit-flag conn nil)
    (websocket-ensure-handshake url conn key protocols nil nil nil)
    ws))

(defun rexel--ws-filter (process output)
  "Pass PROCESS OUTPUT to websocket.el once a whole frame is buffered.
`websocket-outer-filter' copies all buffered input on every read, so
a large frame arriving in many reads would cost quadratic time."
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
      (websocket-outer-filter ws (apply #'concat (nreverse queued))))))

(defun rexel--ws-error (ws type err)
  "Report ERR signalled while running the TYPE callback of WS."
  (message "rexel: %s callback on %s: %S" type (websocket-url ws) err))

(defun rexel--send (conn object)
  "Send OBJECT as one JSON text message on CONN."
  (websocket-send-text (rexel--conn-ws conn) (json-serialize object)))

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

(defun rexel--connect (host callback)
  "Call CALLBACK with a ready connection to HOST, opening one if needed.
HOST is a (LABEL . SOCKET) pair as `rexel--hosts' returns them."
  (let* ((path (cdr host))
         (conn (seq-find (lambda (c) (equal (rexel--conn-socket c) path))
                         rexel--conns)))
    (cond
     ((and conn (websocket-openp (rexel--conn-ws conn))
           (rexel--conn-client-id conn))
      (funcall callback conn))
     ((and conn (websocket-openp (rexel--conn-ws conn)))
      (push callback (rexel--conn-on-ready conn)))
     (t
      (unless (file-exists-p path)
        (user-error "Rex server socket %s does not exist" path))
      (setq conn (rexel--make-conn :socket path :host (car host)))
      (push callback (rexel--conn-on-ready conn))
      (setf (rexel--conn-ws conn)
            (rexel--ws-open
             path "ws://rex/api/control" "rex.control.v1"
             (lambda (_ws)
               (rexel--send conn `((type . "hello")
                                   (api_revision . ,rexel--api-revision)
                                   (client . ((kind . "other")
                                              (name . "rexel")
                                              (version . ,(or (package-get-version)
                                                              "unknown")))))))
             (lambda (_ws frame)
               (rexel--on-control-message conn (websocket-frame-text frame)))
             (lambda (_ws) (rexel--on-control-close conn))))
      (push conn rexel--conns)))))

(defun rexel--on-control-message (conn text)
  "Dispatch the control message TEXT received on CONN."
  (let ((msg (rexel--parse-json text)))
    (pcase (alist-get 'type msg)
      ("welcome"
       (let ((protocol (alist-get 'protocol msg))
             (revision (or (alist-get 'api_revision msg) 0)))
         (if (not (and (equal protocol "rex.control.v1")
                       (eql revision rexel--api-revision)))
             (progn
               (websocket-close (rexel--conn-ws conn))
               (message "rexel: server speaks %s revision %s, rexel needs %s revision %d; update Rex"
                        protocol revision "rex.control.v1" rexel--api-revision))
           (setf (rexel--conn-client-id conn)
                 (or (alist-get 'id (alist-get 'client msg)) t))
           (let ((callbacks (nreverse (rexel--conn-on-ready conn))))
             (setf (rexel--conn-on-ready conn) nil)
             (dolist (callback callbacks) (funcall callback conn))))))
      ("response"
       (let* ((id (alist-get 'id msg))
              (callback (gethash id (rexel--conn-pending conn))))
         (remhash id (rexel--conn-pending conn))
         (when callback
           (funcall callback (alist-get 'payload msg) (alist-get 'error msg)))))
      ("event" (rexel--on-event conn (alist-get 'payload msg))))))

(defun rexel--on-control-close (conn)
  "Forget CONN after its control connection closed.
Its block buffers stay for reading; `rexel-attach' reuses them."
  (setq rexel--conns (delq conn rexel--conns))
  (pcase-dolist (`(,_sid . ,timer) (rexel--conn-refresh-timers conn))
    (cancel-timer timer))
  (dolist (buf (buffer-list))
    (when (eq (buffer-local-value 'rexel--conn buf) conn)
      (rexel--end-stream buf "control connection closed")
      (with-current-buffer buf (setq rexel--conn nil))))
  (message "rexel: control connection to %s closed" (rexel--conn-socket conn)))

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
              (when (buffer-live-p buf) (kill-buffer buf))))
           ("session_view_changed" (rexel--schedule-view-refresh conn sid))
           ("session_destroyed" (rexel--forget-session conn sid)))))
      ("session_destroyed"
       (rexel--forget-session conn (alist-get 'session_id payload))))))

(defun rexel--schedule-view-refresh (conn sid)
  "Refresh session SID's view on CONN shortly, coalescing bursts of events."
  (unless (assoc sid (rexel--conn-refresh-timers conn))
    (push (cons sid (run-at-time 0.1 nil #'rexel--refresh-view conn sid))
          (rexel--conn-refresh-timers conn))))

(defun rexel--refresh-view (conn sid)
  "Fetch session SID's view on CONN and reconcile block buffers."
  (setf (rexel--conn-refresh-timers conn)
        (assoc-delete-all sid (rexel--conn-refresh-timers conn)))
  (when (websocket-openp (rexel--conn-ws conn))
    (rexel--request conn "session.view" `((session_id . ,sid))
                    (lambda (view err)
                      (cond
                       ;; Destroyed while the request was in flight; the
                       ;; session_destroyed event cleans up.
                       ((equal (alist-get 'code err) "session_not_found"))
                       (err (message "rexel: session.view: %s"
                                     (rexel--error-string err)))
                       (t (rexel--apply-view conn sid view)))))))

(defun rexel--forget-session (conn sid)
  "Drop session SID from CONN and kill its block buffers."
  (when-let* ((timer (alist-get sid (rexel--conn-refresh-timers conn)
                                nil nil #'equal)))
    (cancel-timer timer)
    (setf (rexel--conn-refresh-timers conn)
          (assoc-delete-all sid (rexel--conn-refresh-timers conn))))
  (setf (alist-get sid (rexel--conn-sessions conn) nil t #'equal) nil)
  (setf (alist-get sid (rexel--conn-revisions conn) nil t #'equal) nil)
  (dolist (buf (rexel--session-buffers conn sid))
    (kill-buffer buf)))

(defun rexel--session-buffers (_conn sid)
  "Return the block buffers of session SID, streaming or not."
  (seq-filter (lambda (buf)
                (equal (buffer-local-value 'rexel--session-id buf) sid))
              (buffer-list)))


;;; Session views and block buffers

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
  "Where the data connection is: `welcome', `snapshot', `live' or `closed'.")

(defvar-local rexel--requested-size nil
  "Last (COLS . ROWS) requested from the server for this block.")

(defvar-local rexel--size-take nil
  "Non-nil when the next size request takes ownership of the block.")

(defvar-local rexel--size-inflight nil
  "The connection a size request is in flight on, or nil.")

(defvar-local rexel--out-pending nil
  "Bytes typed while a write to the server is in flight.")

(defvar-local rexel--out-inflight nil
  "The connection a `terminal.write' is in flight on, or nil.")

(defun rexel--view-terminal-blocks (view)
  "Return the (BLOCK-ID . LABEL) of every terminal block in VIEW."
  (let (blocks)
    (seq-doseq (window (alist-get 'windows view))
      (seq-doseq (layer (alist-get 'layers window))
        (seq-doseq (block (alist-get 'blocks layer))
          (when (equal (alist-get 'creator_name block) rexel--terminal-creator)
            (push (cons (alist-get 'block_id block) (alist-get 'label block))
                  blocks)))))
    (nreverse blocks)))

(defun rexel--apply-view (conn sid view)
  "Make CONN's block buffers for session SID match VIEW.
Responses can arrive out of order, so a VIEW older than the one
already applied is ignored."
  (let ((revision (alist-get 'revision view))
        (applied (alist-get sid (rexel--conn-revisions conn) nil nil #'equal)))
    (unless (and applied revision (< revision applied))
      (let ((blocks (rexel--view-terminal-blocks view)))
        (setf (alist-get sid (rexel--conn-revisions conn) nil nil #'equal)
              revision)
        (setf (alist-get sid (rexel--conn-sessions conn) nil nil #'equal)
              (alist-get 'label view))
        (pcase-dolist (`(,bid . ,_) blocks)
          (rexel--ensure-block-buffer conn sid bid))
        (dolist (buf (rexel--session-buffers conn sid))
          (with-current-buffer buf
            (if (not (assoc rexel--block-id blocks))
                (kill-buffer buf)
              (let ((name (rexel--buffer-name conn sid rexel--block-id)))
                (unless (equal name (buffer-name))
                  (rename-buffer name t))))))))))

(defun rexel--buffer-name (conn sid bid)
  "Return the buffer name for block BID of session SID on CONN."
  (format "*rex:%s%s:%s*"
          (or (alist-get sid (rexel--conn-sessions conn) nil nil #'equal) "?")
          (if (rexel--conn-host conn) (concat "@" (rexel--conn-host conn)) "")
          (substring bid -6)))

(defun rexel--stale-block-buffer (bid)
  "Return a block buffer for BID left over from an earlier connection."
  (seq-find (lambda (buf)
              (and (equal (buffer-local-value 'rexel--block-id buf) bid)
                   (eq (buffer-local-value 'rexel--data-state buf) 'closed)))
            (buffer-list)))

(defun rexel--ensure-block-buffer (conn sid bid)
  "Return the buffer for block BID of session SID on CONN, creating it.
A buffer whose stream ended earlier is reset and streamed again."
  (let ((buf (gethash bid (rexel--conn-blocks conn))))
    (cond
     ((buffer-live-p buf) buf)
     ((setq buf (rexel--stale-block-buffer bid))
      (ghostel--init-buffer buf 24 80)
      (rexel--attach-block-buffer conn sid bid buf))
     (t
      (setq buf (ghostel--create (rexel--buffer-name conn sid bid) nil 24 80))
      (rexel--attach-block-buffer conn sid bid buf)))))

(defun rexel--attach-block-buffer (conn sid bid buf)
  "Make BUF show block BID of session SID on CONN and return it."
  (puthash bid buf (rexel--conn-blocks conn))
  (with-current-buffer buf
    (setq rexel--conn conn
          rexel--session-id sid
          rexel--block-id bid
          rexel--requested-size nil
          rexel--size-take nil
          rexel--size-inflight nil
          rexel--out-pending nil
          rexel--out-inflight nil
          ghostel-pty-out-function #'rexel--pty-out
          ghostel-resize-function #'rexel--resize)
    (rexel-block-mode 1)
    (add-hook 'window-buffer-change-functions #'rexel--window-shown nil t)
    (add-hook 'kill-buffer-hook #'rexel--block-buffer-killed nil t)
    (rexel--open-data-connection conn buf))
  buf)

(defun rexel--block-buffer-killed ()
  "Close the data connection of the block buffer being killed.
The block itself keeps running on the server."
  (when rexel--conn
    (remhash rexel--block-id (rexel--conn-blocks rexel--conn)))
  (rexel--close-stream))

(defun rexel--pty-out (data)
  "Type DATA, bytes from ghostel's key encoder, into this block.
The server runs block methods concurrently, so writes are sent one
at a time and bytes typed meanwhile are coalesced into the next."
  (if (null rexel--conn)
      (message "rexel: not connected; use M-x rexel-attach")
    (setq rexel--out-pending (concat rexel--out-pending data))
    (rexel--flush-out (current-buffer))))

(defun rexel--flush-out (buf)
  "Send BUF's pending typed bytes unless a write is in flight."
  (with-current-buffer buf
    (when (and rexel--conn
               (not (eq rexel--out-inflight rexel--conn))
               (> (length rexel--out-pending) 0))
      (let ((data rexel--out-pending)
            (conn rexel--conn))
        (setq rexel--out-pending nil)
        (condition-case err
            (progn
              (rexel--request
               conn "com.superlogical.terminal.write"
               `((session_id . ,rexel--session-id)
                 (block_id . ,rexel--block-id)
                 (args . ((data . ,(decode-coding-string data 'utf-8)))))
               (lambda (_result err)
                 (when err
                   (message "rexel: terminal.write: %s" (rexel--error-string err)))
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (setq rexel--out-inflight nil)
                     (rexel--flush-out buf)))))
              (setq rexel--out-inflight conn))
          (error (message "rexel: dropped %d typed bytes: %s"
                          (length data) (error-message-string err))))))))


;;; Data connection

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
           (rexel--open-stream
            (concat "\x01" (base64-decode-string (alist-get 'ticket result) t))))))))))

(defun rexel--open-stream (hello)
  "Connect this buffer's stream to the data endpoint and send HELLO."
  (let ((buf (current-buffer))
        (stream rexel--stream))
    (setf (rexel--stream-ws stream)
          (rexel--ws-open
           (rexel--conn-socket rexel--conn) "ws://rex/api/data"
           "rex.data.terminal.v1"
           (lambda (ws)
             (websocket-send ws (make-websocket-frame
                                 :opcode 'binary :payload hello :completep t)))
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
                 (bindat-pack (bindat-type uint 64 t) (rexel--stream-received s))))
      (error (rexel--on-data-close (current-buffer) s)))))

(defun rexel--close-stream ()
  "Mark this buffer's stream closed and close its WebSocket."
  (setq rexel--data-state 'closed)
  (when rexel--stream
    (ignore-errors (websocket-close (rexel--stream-ws rexel--stream)))
    (setq rexel--stream nil)))

(defun rexel--data-failed (buf reason)
  "Mark BUF's stream as failed for REASON."
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
      (when ghostel--term
        (ghostel--write-vt ghostel--term (format "\r\n[rexel: %s]\r\n" reason))
        (ghostel--invalidate)))))

(defun rexel--on-data-close (buf stream)
  "Note that BUF's data connection STREAM closed.
A connection that drops after its WELCOME is resumed.  Otherwise the
stream ends, and while the control connection is up and the block still
exists, the next view refresh streams the block into BUF again."
  (when (and (buffer-live-p buf)
             (eq stream (buffer-local-value 'rexel--stream buf)))
    (with-current-buffer buf
      (if (memq rexel--data-state '(snapshot live))
          (rexel--resume-stream)
        (rexel--end-stream buf "stream closed")
        (when (and rexel--conn (websocket-openp (rexel--conn-ws rexel--conn))
                   (eq (gethash rexel--block-id (rexel--conn-blocks rexel--conn)) buf))
          (remhash rexel--block-id (rexel--conn-blocks rexel--conn))
          (rexel--schedule-view-refresh rexel--conn rexel--session-id))))))

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

(defun rexel--u64le (bytes offset)
  "Return the little-endian 64-bit integer at OFFSET in BYTES."
  (let ((n 0))
    (dotimes (i 8)
      (setq n (logior n (ash (aref bytes (+ offset i)) (* 8 i)))))
    n))

(defun rexel--on-welcome (bytes)
  "Read the data handshake answer BYTES."
  (cond
   ((and (>= (length bytes) 32) (= (aref bytes 0) #x81))
    (let ((s rexel--stream))
      (setf (rexel--stream-token s) (substring bytes 1 17))
      (if (rexel--stream-resume s)
          (setq rexel--data-state (rexel--stream-resume s))
        (setf (rexel--stream-marker s)
              (format "\e_rex1;%d;" (rexel--u64le bytes 24)))
        (setq rexel--data-state 'snapshot))
      (when (> (length bytes) 32)
        (rexel--on-data (current-buffer) s (substring bytes 32)))))
   ((and (>= (length bytes) 2) (= (aref bytes 0) #x82))
    (rexel--data-failed (current-buffer)
                        (format "data connection rejected, reason %d"
                                (aref bytes 1))))
   (t (rexel--data-failed (current-buffer) "malformed data handshake"))))

(defconst rexel--max-record-bytes (ash 1 28)
  "Snapshot records claiming more bytes than this end the stream.")

(defun rexel--snapshot-walk (bytes pos)
  "Walk the snapshot records in BYTES from the record header at POS.
A header is 10 bytes: a u16 tag and a u32 payload length, then a CRC.
Return (t END) when the FINISH record ends at END.
Otherwise return \(nil END NEED): END is the start of the first
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
         (rest (car (rexel--stream-tail s)))
         (term ghostel--term))
    (setf (rexel--stream-snapshot s) nil
          (rexel--stream-tail s) nil)
    (condition-case err
        (progn
          (ghostel--restore-snapshot term snapshot)
          (setq rexel--data-state 'live))
      (error (rexel--end-stream (current-buffer)
                                (format "bad snapshot: %s"
                                        (error-message-string err)))))
    (when (eq rexel--data-state 'live)
      (ghostel--apply-palette term)
      (ghostel--apply-bold-config term)
      (setq ghostel--force-next-redraw t)
      (ghostel--invalidate)
      (when (> (length rest) 0)
        (rexel--on-live-bytes rest))
      (rexel--sync-size))))

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
MARKER introduces a control message, which ends at ST.  Returns
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
        ;; Callbacks run while parsing may select another buffer.
        (save-current-buffer
          (ghostel--write-vt ghostel--term item))))
    (ghostel--invalidate)))

(defun rexel--handle-message (json)
  "Apply the stream control message JSON."
  (let ((msg (rexel--parse-json (decode-coding-string json 'utf-8))))
    (pcase (alist-get 'type msg)
      ("resize"
       (let ((cols (alist-get 'cols msg))
             (rows (alist-get 'rows msg)))
         (when (and (natnump cols) (natnump rows) (> cols 0) (> rows 0))
           (setq ghostel--term-rows rows
                 ghostel--term-cols cols
                 ghostel--force-next-redraw t)
           (ghostel--set-size-with-cell-dims ghostel--term rows cols))))
      ("child_exited"
       (message "rexel: %s: program exited with status %s"
                (buffer-name) (alist-get 'exit_code msg))))))


;;; Sizing

(defun rexel--resize (rows cols &optional take)
  "Ask the server to size this block ROWS by COLS, if that changes anything.
With TAKE, also take ownership of the block from other clients.  ghostel
calls this as `ghostel-resize-function' with the size fitting the windows
showing the buffer; the terminal itself resizes when the server echoes it."
  (when (and rexel--conn (eq rexel--data-state 'live))
    (let ((size (cons cols rows)))
      (when (or take (not (equal size rexel--requested-size)))
        (setq rexel--requested-size size
              rexel--size-take (or take rexel--size-take))
        (rexel--send-size (current-buffer))))))

(defun rexel--sync-size ()
  "Take this block and size it to the windows showing it, if any."
  (when-let* ((size (ghostel--desired-size)))
    (rexel--resize (max 1 (cdr size)) (max 1 (car size)) t)))

(defun rexel--window-shown (window)
  "Take the block and sync its size after WINDOW started showing it."
  (with-current-buffer (window-buffer window)
    (rexel--sync-size)))

(defun rexel--send-size (buf)
  "Send BUF's requested size, taking the block first if asked to.
Resizes run concurrently on the server, so one is sent at a time and
the latest requested size follows when it completes.  The size applies
once this client owns the block, when the server reports it in the stream."
  (with-current-buffer buf
    (unless (eq rexel--size-inflight rexel--conn)
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
                         (when (or rexel--size-take
                                   (not (equal size rexel--requested-size)))
                           (rexel--send-size buf))))))
             (resize (lambda ()
                       (when (buffer-live-p buf)
                         (setq size (buffer-local-value 'rexel--requested-size buf)))
                       (rexel--request
                        conn "com.superlogical.terminal.resize"
                        `((session_id . ,sid) (block_id . ,bid)
                          (args . ((columns . ,(car size)) (rows . ,(cdr size)))))
                        done))))
        (setq rexel--size-take nil)
        (if (not take)
            (funcall resize)
          (rexel--request
           conn "session.own_block"
           `((session_id . ,sid) (block_ids . ,(vector bid)) (mode . "take"))
           (lambda (result err)
             (if err
                 (funcall done result err)
               (funcall resize)))))
        (setq rexel--size-inflight conn)))))


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
         (cond
          ((null sessions) (message "rexel: the server has no sessions"))
          ((null (cdr sessions)) (funcall callback (cdar sessions)))
          (t (funcall callback
                      (cdr (assoc (completing-read "Rex session: " sessions
                                                   nil t)
                                  sessions))))))))))

(defun rexel--attach-session (conn sid)
  "Attach CONN to session SID and show its focused block."
  (rexel--request
   conn "session.attach" `((session_id . ,sid))
   (lambda (result err)
     (if err
         (message "rexel: session.attach: %s" (rexel--error-string err))
       (let ((view (alist-get 'view result)))
         (rexel--apply-view conn sid view)
         (rexel--show-focused-block conn view))))))

(defun rexel--show-focused-block (conn view)
  "Pop to the buffer of VIEW's focused block on CONN."
  (let* ((active (alist-get 'active_window_id view))
         (window (seq-find (lambda (w) (equal (alist-get 'window_id w) active))
                           (alist-get 'windows view)))
         (bid (and window (alist-get 'focused_block_id window)))
         (buf (and bid (gethash bid (rexel--conn-blocks conn)))))
    (when (buffer-live-p buf)
      (pop-to-buffer buf))))

;;;###autoload
(defun rexel-attach ()
  "Attach to a Rex session and show its terminals as ghostel buffers."
  (interactive)
  (let* ((hosts (rexel--hosts))
         (names (mapcar (lambda (h) (cons (or (car h) "this machine") h)) hosts)))
    (rexel--connect
     (cond ((null hosts) (user-error "No Rex server found; see `rexel-hosts'"))
           ((null (cdr hosts)) (car hosts))
           (t (cdr (assoc (completing-read "Rex host: " names nil t) names))))
     (lambda (conn)
       (rexel--pick-session conn (lambda (sid) (rexel--attach-session conn sid)))))))

(defun rexel--ensure-block-buffer-context ()
  "Signal an error unless the current buffer shows a live Rex block."
  (unless (and rexel--conn rexel--block-id)
    (user-error "Not in a connected Rex block buffer")))

(defun rexel-detach ()
  "Detach from the current buffer's session and kill its block buffers."
  (interactive)
  (rexel--ensure-block-buffer-context)
  (let ((conn rexel--conn)
        (sid rexel--session-id))
    (rexel--request conn "session.detach" `((session_id . ,sid)))
    (rexel--forget-session conn sid)))

(defun rexel--new-shell-layout (&optional label)
  "Return a layout object holding one shell block, labelled LABEL."
  `((block . ((flavor . ,rexel--shell-flavor)
              ,@(when label `((label . ,label)))))))

(defun rexel--show-new-blocks (conn sid result err method)
  "Show the first block METHOD created on CONN in session SID.
RESULT and ERR are the response."
  (if err
      (message "rexel: %s: %s" method (rexel--error-string err))
    (let ((bid (seq-first (alist-get 'block_ids result))))
      (when bid
        (pop-to-buffer (rexel--ensure-block-buffer conn sid bid))))))

(defun rexel-split (&optional below)
  "Split the current block with a new shell to the right, or BELOW with prefix."
  (interactive "P")
  (rexel--ensure-block-buffer-context)
  (let ((conn rexel--conn)
        (sid rexel--session-id))
    (rexel--request
     conn "session.new_split"
     `((session_id . ,sid) (anchor_id . ,rexel--block-id)
       (direction . ,(if below "vertical" "horizontal")) (side . "after")
       (layout . ,(rexel--new-shell-layout)) (focus . t))
     (lambda (result err)
       (rexel--show-new-blocks conn sid result err "session.new_split")))))

(defun rexel-split-below ()
  "Split the current block with a new shell below it."
  (interactive)
  (rexel-split t))

(defun rexel-new-window (label)
  "Open a new window holding one shell in the current session, labelled LABEL."
  (interactive "sWindow label: ")
  (rexel--ensure-block-buffer-context)
  (let ((conn rexel--conn)
        (sid rexel--session-id))
    (rexel--request
     conn "session.new_window"
     `((session_id . ,sid) (layout . ,(rexel--new-shell-layout))
       ,@(unless (string-empty-p label) `((window_label . ,label))))
     (lambda (result err)
       (rexel--show-new-blocks conn sid result err "session.new_window")))))

(defun rexel-close-block ()
  "Close the current block on the server, ending its program."
  (interactive)
  (rexel--ensure-block-buffer-context)
  (when (yes-or-no-p (format "Close Rex block %s? " (buffer-name)))
    (rexel--request rexel--conn "block.close"
                    `((session_id . ,rexel--session-id)
                      (block_id . ,rexel--block-id)))))

(defun rexel-send-prefix ()
  "Send `rexel-prefix-key' itself to the terminal."
  (interactive)
  (let ((spec (ghostel--event-key-spec (aref (kbd rexel-prefix-key) 0))))
    (when spec
      (ghostel--on-user-input)
      (ghostel--send-encoded (car spec) (cdr spec)))))

(defvar rexel-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "%" #'rexel-split)
    (define-key map "\"" #'rexel-split-below)
    (define-key map "c" #'rexel-new-window)
    (define-key map "x" #'rexel-close-block)
    (define-key map "d" #'rexel-detach)
    (define-key map (kbd rexel-prefix-key) #'rexel-send-prefix)
    map)
  "Commands available behind `rexel-prefix-key' in block buffers.")

(defvar rexel-block-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd rexel-prefix-key) rexel-prefix-map)
    map)
  "Keymap of `rexel-block-mode'; binds `rexel-prefix-key'.")

(define-minor-mode rexel-block-mode
  "Minor mode of ghostel buffers showing a Rex block."
  :lighter " Rex"
  :keymap rexel-block-mode-map)

(provide 'rexel)
;;; rexel.el ends here
