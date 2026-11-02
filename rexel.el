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
;; Both connections are WebSockets on the server's Unix socket, found
;; through `rexel-program' or `rexel-server-socket'.

;;; Code:

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
  "The rex command-line program, used to locate the server socket."
  :type 'string)

(defcustom rexel-server-socket nil
  "Path of the Rex server's Unix socket, or nil to ask `rexel-program'.
Without the program the platform default is used."
  :type '(choice (const :tag "Ask rex" nil) file))

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
  "One control connection to a Rex server."
  ws socket client-id
  (next-id 0)
  (pending (make-hash-table :test #'equal))
  (sessions nil)
  (revisions nil)
  (blocks (make-hash-table :test #'equal))
  (on-ready nil)
  (refresh-timers nil))

(defvar rexel--conn nil
  "The current server connection, or nil.")

(defun rexel--socket-path ()
  "Return the server's Unix socket path."
  (or rexel-server-socket
      (when-let* ((rex (executable-find rexel-program))
                  (status (with-temp-buffer
                            (when (zerop (call-process rex nil t nil
                                                       "server" "status" "--json"))
                              (buffer-string))))
                  (endpoint (alist-get 'endpoint (rexel--parse-json status)))
                  ((string-prefix-p "unix://" endpoint)))
        (url-unhex-string (substring endpoint (length "unix://"))))
      (if (eq system-type 'darwin)
          (expand-file-name "~/Library/Application Support/rex/server.sock")
        "/run/rex/server.sock")))

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

(defun rexel--connect (callback)
  "Call CALLBACK with a ready connection, opening one if needed."
  (let ((conn rexel--conn))
    (cond
     ((and conn (websocket-openp (rexel--conn-ws conn))
           (rexel--conn-client-id conn))
      (funcall callback conn))
     ((and conn (websocket-openp (rexel--conn-ws conn)))
      (push callback (rexel--conn-on-ready conn)))
     (t
      (let* ((path (rexel--socket-path))
             (conn (rexel--make-conn :socket path)))
        (unless (file-exists-p path)
          (user-error "Rex server socket %s does not exist" path))
        (push callback (rexel--conn-on-ready conn))
        (setq rexel--conn conn)
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
               (lambda (_ws) (rexel--on-control-close conn)))))))))

(defun rexel--on-control-message (conn text)
  "Dispatch the control message TEXT received on CONN."
  (let ((msg (rexel--parse-json text)))
    (pcase (alist-get 'type msg)
      ("welcome"
       (unless (equal (alist-get 'protocol msg) "rex.control.v1")
         (message "rexel: unexpected control protocol %s" (alist-get 'protocol msg)))
       (setf (rexel--conn-client-id conn)
             (or (alist-get 'id (alist-get 'client msg)) t))
       (let ((callbacks (nreverse (rexel--conn-on-ready conn))))
         (setf (rexel--conn-on-ready conn) nil)
         (dolist (callback callbacks) (funcall callback conn))))
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
  (when (eq conn rexel--conn)
    (setq rexel--conn nil))
  (pcase-dolist (`(,_sid . ,timer) (rexel--conn-refresh-timers conn))
    (cancel-timer timer))
  (maphash (lambda (_bid buf)
             (rexel--end-stream buf "control connection closed"))
           (rexel--conn-blocks conn))
  (message "rexel: control connection to %s closed" (rexel--conn-socket conn)))

(defun rexel--on-event (conn event)
  "Handle EVENT received on CONN."
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

(defvar-local rexel--data-ws nil
  "WebSocket streaming this block's terminal.")

(defvar-local rexel--data-state nil
  "Where the data connection is: `welcome', `snapshot', `live' or `closed'.")

(defvar-local rexel--pending ""
  "Live stream bytes not yet consumed: a partial control message.")

(defvar-local rexel--snapshot nil
  "The complete snapshot chunks received so far, newest first.")

(defvar-local rexel--snapshot-tail nil
  "Chunks of the incomplete snapshot record, newest first.")

(defvar-local rexel--snapshot-need 10
  "Bytes `rexel--snapshot-tail' needs before it is walked again.")

(defvar-local rexel--marker nil
  "Prefix of this stream's control messages, including the channel nonce.")

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
          (unless (assoc (buffer-local-value 'rexel--block-id buf) blocks)
            (kill-buffer buf)))))))

(defun rexel--buffer-name (conn sid bid)
  "Return the buffer name for block BID of session SID on CONN."
  (format "*rex:%s:%s*"
          (or (alist-get sid (rexel--conn-sessions conn) nil nil #'equal) "?")
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
    (setq rexel--session-id sid
          rexel--block-id bid
          rexel--requested-size nil
          rexel--size-take nil
          rexel--size-inflight nil
          rexel--out-pending nil
          rexel--out-inflight nil
          ghostel--pty-out-function #'rexel--pty-out)
    (rexel-block-mode 1)
    (add-hook 'window-size-change-functions #'rexel--window-resized nil t)
    (add-hook 'window-buffer-change-functions #'rexel--window-shown nil t)
    (add-hook 'kill-buffer-hook #'rexel--block-buffer-killed nil t)
    (rexel--open-data-connection conn buf))
  buf)

(defun rexel--block-buffer-killed ()
  "Close the data connection of the block buffer being killed.
The block itself keeps running on the server."
  (when rexel--conn
    (remhash rexel--block-id (rexel--conn-blocks rexel--conn)))
  (setq rexel--data-state 'closed)
  (when rexel--data-ws
    (ignore-errors (websocket-close rexel--data-ws))))

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
         (let ((ticket (base64-decode-string (alist-get 'ticket result) t)))
           (with-current-buffer buf
             (setq rexel--data-state 'welcome
                   rexel--pending ""
                   rexel--snapshot nil
                   rexel--snapshot-tail nil
                   rexel--snapshot-need 10)
             (setq rexel--data-ws
                   (rexel--ws-open
                    (rexel--conn-socket conn) "ws://rex/api/data"
                    "rex.data.terminal.v1"
                    (lambda (ws)
                      (websocket-send ws (make-websocket-frame
                                          :opcode 'binary
                                          :payload (concat "\x01" ticket)
                                          :completep t)))
                    (lambda (_ws frame)
                      (rexel--on-data buf (websocket-frame-payload frame)))
                    (lambda (_ws) (rexel--on-data-close buf))))))))))))

(defun rexel--data-failed (buf reason)
  "Mark BUF's stream as failed for REASON."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setq rexel--data-state 'closed)
      (message "rexel: %s: %s" (buffer-name) reason))))

(defun rexel--end-stream (buf reason)
  "End BUF's stream, noting REASON in the terminal."
  (when (and (buffer-live-p buf)
             (not (eq (buffer-local-value 'rexel--data-state buf) 'closed)))
    (with-current-buffer buf
      (setq rexel--data-state 'closed
            rexel--pending ""
            rexel--snapshot nil
            rexel--snapshot-tail nil)
      (when rexel--data-ws
        (ignore-errors (websocket-close rexel--data-ws)))
      (when ghostel--term
        (ghostel--write-vt ghostel--term (format "\r\n[rexel: %s]\r\n" reason))
        (ghostel--invalidate)))))

(defun rexel--on-data-close (buf)
  "Note that BUF's data connection closed.
While the control connection is up and the block still exists, the
next view refresh streams the block into BUF again."
  (when (and (buffer-live-p buf)
             (not (eq (buffer-local-value 'rexel--data-state buf) 'closed)))
    (rexel--end-stream buf "stream closed")
    (with-current-buffer buf
      (when (and rexel--conn (websocket-openp (rexel--conn-ws rexel--conn))
                 (eq (gethash rexel--block-id (rexel--conn-blocks rexel--conn)) buf))
        (remhash rexel--block-id (rexel--conn-blocks rexel--conn))
        (rexel--schedule-view-refresh rexel--conn rexel--session-id)))))

(defun rexel--on-data (buf bytes)
  "Handle BYTES, one binary message of BUF's data connection."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (pcase rexel--data-state
        ('welcome (rexel--on-welcome bytes))
        ('snapshot (rexel--on-snapshot-bytes bytes))
        ('live (rexel--on-live-bytes bytes))))))

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
    (setq rexel--marker (format "\e_rex1;%d;" (rexel--u64le bytes 24))
          rexel--data-state 'snapshot)
    (when (> (length bytes) 32)
      (rexel--on-snapshot-bytes (substring bytes 32))))
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
Return (t END) when the FINISH record ends at END.  Otherwise return
\(nil END NEED): END is the start of the first incomplete record and
NEED the bytes from END that complete it, or its header if that is
cut off.  Signal an error on a record longer than
`rexel--max-record-bytes'."
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
  "Collect snapshot BYTES and restore the terminal once it is complete.
Complete records collect in `rexel--snapshot'.  Chunks of the
incomplete record after them collect in `rexel--snapshot-tail' and
are walked again once `rexel--snapshot-need' bytes have arrived."
  (push bytes rexel--snapshot-tail)
  (when (>= (apply #'+ (mapcar #'length rexel--snapshot-tail))
            rexel--snapshot-need)
    (let* ((data (apply #'concat (nreverse rexel--snapshot-tail)))
           (walk (if (and (null rexel--snapshot)
                          (not (string-prefix-p "GHOSTSNP" data)))
                     "stream is not a terminal snapshot"
                   (condition-case err
                       (rexel--snapshot-walk data (if rexel--snapshot 0 10))
                     (error (error-message-string err))))))
      (if (stringp walk)
          (rexel--end-stream (current-buffer) walk)
        (pcase-let ((`(,finished ,end ,need) walk))
          (when (> end 0)
            (push (substring data 0 end) rexel--snapshot))
          (setq rexel--snapshot-tail (list (substring data end))
                rexel--snapshot-need need)
          (when finished
            (rexel--restore-snapshot)))))))

(defun rexel--restore-snapshot ()
  "Restore the collected snapshot and go live with the bytes after it."
  (let ((snapshot (apply #'concat (nreverse rexel--snapshot)))
        (rest (car rexel--snapshot-tail))
        (term ghostel--term))
    (setq rexel--snapshot nil
          rexel--snapshot-tail nil)
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
      (rexel--sync-size t))))

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
  (pcase-let ((`(,items . ,pending)
               (rexel--split-stream (concat rexel--pending bytes) rexel--marker)))
    (setq rexel--pending pending)
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

(defun rexel--desired-size ()
  "Return (COLS . ROWS) fitting every window showing this buffer, or nil."
  (when-let* ((windows (ghostel--windows (current-buffer) t))
              (size (window-adjust-process-window-size-smallest nil windows)))
    (and (> (car size) 0) (> (cdr size) 0) size)))

(defun rexel--sync-size (&optional take)
  "Ask the server to size this block to the windows showing it, if needed.
With TAKE, also take ownership of the block from other clients."
  (when (and rexel--conn (eq rexel--data-state 'live))
    (let ((size (rexel--desired-size)))
      (when (and size (or take (not (equal size rexel--requested-size))))
        (setq rexel--requested-size size
              rexel--size-take (or take rexel--size-take))
        (rexel--send-size (current-buffer))))))

(defun rexel--window-resized (window)
  "Sync the block size after WINDOW, showing a block buffer, changed size."
  (with-current-buffer (window-buffer window)
    (rexel--sync-size)))

(defun rexel--window-shown (window)
  "Take the block and sync its size after WINDOW started showing it."
  (with-current-buffer (window-buffer window)
    (rexel--sync-size t)))

(defun rexel--send-size (buf)
  "Send BUF's requested size, taking the block first if asked to.
Resizes run concurrently on the server, so one is sent at a time and
the latest requested size follows when it completes.  The size applies
once this client owns the block, when the server reports it in the
stream."
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
  (rexel--connect
   (lambda (conn)
     (rexel--pick-session conn (lambda (sid) (rexel--attach-session conn sid))))))

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
