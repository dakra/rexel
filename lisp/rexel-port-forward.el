;;; rexel-port-forward.el --- Port forwards for rexel -*- lexical-binding: t; -*-

;; Author: Daniel Kraus <daniel@kraus.my>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; TCP ports of the machine a Rex block runs on, forwarded to this one.
;; `rexel-port-forward' listens on a local port and relays each
;; connection to it through a tunnel block: a block without a view that
;; the server keeps in the session and that connects to the target for
;; every data stream its owner opens.  The server lists the ports the
;; programs of a terminal listen on and says when they change;
;; `rexel-port-forward-new-ports' and
;; `rexel-port-forward-when-port-closes' say what follows from that.  A
;; link to localhost in a block buffer opens through a forward, see
;; `rexel-port-forward-links'.  rexel.el loads this file.

;;; Code:

(require 'browse-url)
(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'websocket)
(require 'rexel-transfer)

(defvar rexel--conn)
(defvar rexel--session-id)
(defvar rexel--block-id)
(defvar rexel--exit-timeout)
(defvar rexel--writes-overdue)

(declare-function rexel--conn-host "rexel" (cl-x) t)
(declare-function rexel--conn-attaching "rexel" (cl-x) t)
(declare-function rexel--conn-name "rexel" (conn))
(declare-function rexel--ready-p "rexel" (conn))
(declare-function rexel--request "rexel" (conn method payload &optional callback))
(declare-function rexel--error-string "rexel" (err))
(declare-function rexel--watched-write "rexel" (process on-stall write &optional size))
(declare-function rexel--open-data-ws "rexel" (protocol hello on-message on-close))
(declare-function rexel--hello "rexel" (ticket))
(declare-function rexel--handshake "rexel" (bytes))
(declare-function rexel--prompt-later "rexel" (function))
(declare-function rexel--choose "rexel" (prompt alist))
(declare-function rexel--ensure-block-buffer-context "rexel" ())

(defcustom rexel-port-forward-new-ports 'message
  "What happens when a program in a block starts to listen on a port.
`message' names the port in the echo area, t forwards it as
`rexel-port-forward' does, and nil does nothing.  This holds for the
blocks of servers on other machines, and for a port opened while rexel
is attached to the block's session."
  :type '(choice (const :tag "Name it in the echo area" message)
                 (const :tag "Forward it" t)
                 (const :tag "Nothing" nil))
  :group 'rexel)

(defcustom rexel-port-forward-when-port-closes 'remember
  "What happens to a forward when its block stops listening on the port.
`remember' closes the forward and opens it again, on the same local port
when that is free, once the block listens on the port again.  `close'
closes it for good, and `keep' leaves it open.  This holds for a port
the server listed for the block; a forward to another port or host stays
until `rexel-port-forward-close' or the end of its block."
  :type '(choice (const :tag "Close it, open it again with the port" remember)
                 (const :tag "Close it" close)
                 (const :tag "Keep it" keep))
  :group 'rexel)

(defcustom rexel-port-forward-links t
  "Whether a link to localhost in a block buffer opens through a forward.
With non-nil, opening an http or https link to localhost in a block of a
server on another machine forwards the link's port, as
`rexel-port-forward' does, and opens the link on the local port."
  :type 'boolean
  :group 'rexel)


;;; Forwards

(cl-defstruct (rexel--forward (:constructor rexel--make-forward) (:copier nil))
  "A local port forwarded to PORT on HOST, as the server of CONN reaches it.
BUFFER is the block buffer it was opened from and SESSION that block's
session, which holds the tunnel block BLOCK, nil until the server made
it.  MAKING is the mark of the request for BLOCK while it is unanswered,
THEN what to call once a forward closed meanwhile has closed the block
it gets, and TAKING non-nil while BLOCK is taken back after a redial.
LISTENER is the process listening on LOCAL-PORT and RELAYS are the
connections it accepted."
  buffer conn session host port listener local-port block making then taking
  relays)

(cl-defstruct (rexel--relay (:constructor rexel--make-relay) (:copier nil))
  "One connection to the local port of FORWARD, relayed over a data stream.
CLIENT is the process of the local connection and WS the WebSocket of
the stream.  STATE is nil until the tunnel block exists, `begin' while
the ticket is requested, `welcome' until the server answers the HELLO,
then `open' and at last `closed'.  OUT holds what waits to be written to
the stream and IN what waits for CLIENT, oldest first, each item bytes
or the symbol `eof'.  BACKLOG is the bytes CLIENT sent that are not
written yet.  BUSY has `out' or `in' while that side is written or a
timer is about to write it."
  forward client ws state out in (backlog 0) busy)

(defconst rexel--relay-backlog-bytes (* 1024 1024)
  "Bytes a relay holds for its stream before it stops reading its connection.")

(defconst rexel--relays-max 100
  "Connections a forward relays at a time; one more is refused.
A forward whose target is its own local port, as one to a free port of a
server on this machine is, would relay each connection into the next.")

(defvar rexel--forwards nil
  "The open `rexel--forward's, newest first.")

(defvar-local rexel--forward-remembered nil
  "The (PORT . LOCAL-PORT) of this block's forwards that closed with their port.
One is opened again when the block listens on its port again.")

(defconst rexel--tunnel-creator "com.superlogical.tunnel"
  "Creator name of Rex tunnel blocks.")

(defconst rexel--loopback-addresses '("127.0.0.1" "::1" "0.0.0.0" "::")
  "Addresses a port is bound to when the machine's localhost reaches it.")

(defun rexel--forward-find (conn host port)
  "Return the forward to PORT on HOST through CONN, or nil."
  (seq-find (lambda (forward)
              (and (eq conn (rexel--forward-conn forward))
                   (eql port (rexel--forward-port forward))
                   (equal host (rexel--forward-host forward))))
            rexel--forwards))

(defun rexel--forward-url (forward)
  "Return the URL of the local port of FORWARD."
  (format "http://localhost:%d/" (rexel--forward-local-port forward)))

(defun rexel--forward-target (forward)
  "Return the target of FORWARD as \"port N\", or \"HOST:N\" on another host."
  (if (equal (rexel--forward-host forward) "localhost")
      (format "port %d" (rexel--forward-port forward))
    (format "%s:%d" (rexel--forward-host forward) (rexel--forward-port forward))))

(defun rexel--forward-describe (forward)
  "Return a line saying what FORWARD forwards."
  (format "%s forwards to %s on %s"
          (rexel--forward-url forward) (rexel--forward-target forward)
          (rexel--conn-name (rexel--forward-conn forward))))

(defun rexel--forward-label (forward)
  "Return the label of the tunnel block of FORWARD.
`rexel--forward-close-leftovers' knows a block of rexel by its start."
  (format "rexel %d:%s:%d" (rexel--forward-local-port forward)
          (rexel--forward-host forward) (rexel--forward-port forward)))

(defun rexel--forward-port-free-p (port)
  "Return non-nil if nothing accepts connections on PORT of this machine."
  (not (or
        ;; A forward would relay a connection to its port.
        (seq-some (lambda (forward)
                    (eql port (rexel--forward-local-port forward)))
                  rexel--forwards)
        ;; Emacs binds with SO_REUSEADDR: a listener on 127.0.0.1 takes the
        ;; connections of one on every address, and a program on ::1 alone
        ;; those to localhost, which a forward's URL names.
        (seq-some (lambda (address)
                    (ignore-errors
                      (delete-process
                       (make-network-process
                        :name "rexel-probe" :host (car address) :service port
                        :family (cdr address) :noquery t))
                      t))
                  '(("127.0.0.1" . ipv4) ("::1" . ipv6))))))

(defun rexel--forward-listen (forward preferred)
  "Return a process listening on 127.0.0.1 for the connections of FORWARD.
It listens on port PREFERRED, or on a free port when that one is taken."
  (cl-flet ((listen (service)
              (make-network-process
               :name (format "rexel forward %s:%d" (rexel--forward-host forward)
                             (rexel--forward-port forward))
               :server t :host "127.0.0.1" :family 'ipv4 :service service
               :coding 'binary :noquery t :plist (list :forward forward)
               :filter #'rexel--relay-filter
               :sentinel #'rexel--relay-sentinel)))
    (or (and (rexel--forward-port-free-p preferred)
             (ignore-errors (listen preferred)))
        (listen t))))

(defun rexel--forward-open (host port &optional local)
  "Forward a port of this machine to PORT on HOST; return the forward.
HOST is as the server of this buffer's block reaches it.  The local port
is LOCAL, by default PORT, or a free one when that is taken.  A forward
the connection has to the target already is returned as it is."
  (or (rexel--forward-find rexel--conn host port)
      (let* ((forward (rexel--make-forward
                       :buffer (current-buffer) :conn rexel--conn
                       :session rexel--session-id :host host :port port))
             (listener (rexel--forward-listen forward (or local port))))
        (setf (rexel--forward-listener forward) listener
              (rexel--forward-local-port forward)
              (process-contact listener :service))
        (push forward rexel--forwards)
        (rexel--forward-make-block forward)
        forward)))

(defun rexel--forward-make-block (forward)
  "Have the server make the tunnel block of FORWARD.
The connections accepted meanwhile are relayed once it exists."
  (let ((conn (rexel--forward-conn forward))
        (sid (rexel--forward-session forward))
        (mark (list nil)))
    (setf (rexel--forward-block forward) nil
          (rexel--forward-taking forward) nil
          (rexel--forward-making forward) mark)
    (rexel--request
     conn "session.new_block"
     `((session_id . ,sid)
       (flavor . ,(concat rexel--tunnel-creator ".dial"))
       (options . ((host . ,(rexel--forward-host forward))
                   (port . ,(rexel--forward-port forward))
                   (client_address
                    . ,(format "127.0.0.1:%d" (rexel--forward-local-port forward)))))
       (label . ,(rexel--forward-label forward)))
     (lambda (result err)
       (let ((bid (alist-get 'block_id result))
             (current (eq mark (rexel--forward-making forward))))
         (when current
           (setf (rexel--forward-making forward) nil))
         (cond
          ;; The forward closed meanwhile, or a later request makes its block.
          ((not (and current (memq forward rexel--forwards)))
           (let ((then (and current
                            (cl-shiftf (rexel--forward-then forward) nil))))
             (cond (bid (rexel--forward-close-block conn sid bid then))
                   (then (funcall then)))))
          (err
           (message "rexel: cannot forward to %s: %s"
                    (rexel--forward-target forward) (rexel--error-string err))
           (rexel--forward-close forward))
          (t
           (setf (rexel--forward-block forward) bid)
           (rexel--forward-start-relays forward))))))))

(defun rexel--forward-ready-p (forward)
  "Return non-nil when a stream of FORWARD can be opened.
It needs the tunnel block, owned by this client, and a connection
attached to the block's session."
  (let ((conn (rexel--forward-conn forward)))
    (and (rexel--forward-block forward)
         (not (rexel--forward-taking forward))
         (rexel--ready-p conn)
         (not (member (rexel--forward-session forward)
                      (rexel--conn-attaching conn))))))

(defun rexel--forward-start-relays (forward)
  "Start the relays of FORWARD that wait for a stream, once one can open."
  (when (rexel--forward-ready-p forward)
    (dolist (relay (reverse (rexel--forward-relays forward)))
      (unless (rexel--relay-state relay)
        (rexel--relay-start relay)))))

(defun rexel--forward-close-block (conn sid bid &optional then)
  "Close the tunnel block BID of session SID on CONN, then call THEN.
THEN is called at once when CONN cannot take the request."
  (let ((then (or then #'ignore)))
    (if (not (rexel--ready-p conn))
        (funcall then)
      (condition-case nil
          (rexel--request conn "block.close"
                          `((session_id . ,sid) (block_id . ,bid))
                          (lambda (&rest _) (funcall then)))
        (error (funcall then))))))

(defun rexel--forward-close (forward &optional then)
  "Close FORWARD: its listener, its connections and its tunnel block.
Call THEN once the server has answered, or at once without a block.  A block
still being made is closed when it arrives, and THEN called after that."
  (if (not (memq forward rexel--forwards))
      (when then (funcall then))
    (setq rexel--forwards (delq forward rexel--forwards))
    (when (process-live-p (rexel--forward-listener forward))
      (delete-process (rexel--forward-listener forward)))
    (mapc #'rexel--relay-close (copy-sequence (rexel--forward-relays forward)))
    (let ((bid (rexel--forward-block forward))
          (conn (rexel--forward-conn forward)))
      (cond (bid (rexel--forward-close-block
                  conn (rexel--forward-session forward) bid then))
            ((and (rexel--forward-making forward) (rexel--ready-p conn))
             (setf (rexel--forward-then forward) then))
            (then (funcall then))))))

(defun rexel--forwards-close (forwards &optional then)
  "Close FORWARDS and call THEN once the server has answered for each."
  (let ((pending (1+ (length forwards))))
    (cl-flet ((done ()
                (when (and (zerop (cl-decf pending)) then)
                  (funcall then))))
      (dolist (forward forwards)
        (rexel--forward-close forward #'done))
      (done))))

(defun rexel--buffer-forwards (buf)
  "Return the forwards opened from the block buffer BUF."
  (seq-filter (lambda (forward) (eq buf (rexel--forward-buffer forward)))
              rexel--forwards))

(defun rexel--session-forwards (conn sid)
  "Return the forwards whose tunnel blocks session SID on CONN holds."
  (seq-filter (lambda (forward)
                (and (eq conn (rexel--forward-conn forward))
                     (equal sid (rexel--forward-session forward))))
              rexel--forwards))

(defun rexel--forward-block-closed (conn bid)
  "Close the forward on CONN whose tunnel block BID closed on the server."
  (when-let* ((forward (seq-find
                        (lambda (forward)
                          (and (eq conn (rexel--forward-conn forward))
                               (equal bid (rexel--forward-block forward))))
                        rexel--forwards)))
    (setf (rexel--forward-block forward) nil)
    (rexel--forward-close forward)
    (message "rexel: the forward to %s on %s was closed on the server"
             (rexel--forward-target forward) (rexel--conn-name conn))))

(defun rexel--forwards-resume (conn sid)
  "Take back the tunnel blocks of session SID on CONN after a redial.
A block that cannot be taken is closed, if the server still has it, and
made again, as is one whose request went with the connection.  The
relays whose ticket was requested on the dropped connection ask again."
  (dolist (forward (rexel--session-forwards conn sid))
    (dolist (relay (rexel--forward-relays forward))
      (when (eq (rexel--relay-state relay) 'begin)
        (setf (rexel--relay-state relay) nil)))
    (if-let* ((bid (rexel--forward-block forward)))
        (progn
          ;; A tunnel has no owner once its owner's connection dropped, and
          ;; only its owner may open its streams.
          (setf (rexel--forward-taking forward) t)
          (rexel--request
           conn "session.own_block"
           `((session_id . ,sid) (block_ids . ,(vector bid)) (mode . "take"))
           (lambda (_result err)
             (when (and (memq forward rexel--forwards)
                        (equal bid (rexel--forward-block forward)))
               (setf (rexel--forward-taking forward) nil)
               (if (not err)
                   (rexel--forward-start-relays forward)
                 (rexel--forward-close-block conn sid bid)
                 (rexel--forward-make-block forward))))))
      (rexel--forward-make-block forward))))

(defun rexel--forward-close-leftovers (conn sid)
  "Close the tunnel blocks a rexel left behind in session SID on CONN.
Those have a label as `rexel--forward-label' makes it and no owner: a
tunnel loses its owner when the owner's connection ends.  After a redial
they include this client's own: the block of a forward closed while the
connection was down, and one whose making the server never confirmed."
  (when (rexel--conn-host conn)
    (rexel--request
     conn "session.list_blocks" `((session_id . ,sid))
     (lambda (result _err)
       (seq-doseq (block (alist-get 'blocks result))
         (let ((bid (alist-get 'block_id block)))
           (when (and (equal (alist-get 'creator_name block) rexel--tunnel-creator)
                      (string-prefix-p "rexel " (or (alist-get 'label block) ""))
                      (null (alist-get 'owner block))
                      (not (seq-some (lambda (forward)
                                       (equal bid (rexel--forward-block forward)))
                                     rexel--forwards)))
             (rexel--forward-close-block conn sid bid))))))))

(defun rexel--forwards-leave ()
  "Close the tunnel blocks of all forwards, for an exiting Emacs.
Wait up to `rexel--exit-timeout' seconds for the server's answers."
  (when rexel--forwards
    (let ((end (+ (float-time) rexel--exit-timeout))
          done)
      (rexel--forwards-close (copy-sequence rexel--forwards)
                             (lambda () (setq done t)))
      (while (and (not done) (< (float-time) end))
        (accept-process-output nil 0.05)))))


;;; Relays

(defun rexel--relay-of (process)
  "Return the relay of PROCESS, a connection a forward's listener accepted.
A new one is started when its forward can open a stream, and waits for
that otherwise.  A connection to a forward that closed, or that relays
`rexel--relays-max' already, is closed and has no relay."
  (or (process-get process :relay)
      (let ((forward (process-get process :forward)))
        (if (or (not (memq forward rexel--forwards))
                (>= (length (rexel--forward-relays forward)) rexel--relays-max))
            (ignore (delete-process process))
          (let ((relay (rexel--make-relay :forward forward :client process)))
            (process-put process :relay relay)
            (push relay (rexel--forward-relays forward))
            (when (rexel--forward-ready-p forward)
              (rexel--relay-start relay))
            relay)))))

(defun rexel--relay-sentinel (process _change)
  "Start the relay of an accepted PROCESS and end the one of a closed one.
Emacs closes a connection whose peer ends its side, so what the target
still sends then is lost."
  (when-let* ((forward (process-get process :forward))
              ((not (eq process (rexel--forward-listener forward)))))
    (if (process-live-p process)
        (rexel--relay-of process)
      (when-let* ((relay (process-get process :relay)))
        (rexel--relay-queue relay 'out 'eof)))))

(defun rexel--relay-filter (process bytes)
  "Send BYTES, which the local connection PROCESS sent, to its stream.
They go in messages a Unix socket's send buffer takes at once.  A connection
far ahead of its stream is not read until the stream has caught up."
  (when-let* ((relay (rexel--relay-of process)))
    (when (> (cl-incf (rexel--relay-backlog relay) (length bytes))
             rexel--relay-backlog-bytes)
      (set-process-filter process t))
    (apply #'rexel--relay-queue relay 'out
           (seq-partition bytes rexel--upload-chunk-bytes))))

(defun rexel--relay-start (relay)
  "Ask for the data stream of RELAY and connect it."
  (let ((forward (rexel--relay-forward relay)))
    (setf (rexel--relay-state relay) 'begin)
    (condition-case nil
        (rexel--request
         (rexel--forward-conn forward) "data.connect"
         `((session_id . ,(rexel--forward-session forward))
           (block_id . ,(rexel--forward-block forward))
           (data_ref_id . ,(concat "conn:" (rexel--uuid)))
           (subprotocol . "rex.data.bytes.v1"))
         (lambda (result err)
           (cond
            ((eq (rexel--relay-state relay) 'closed))
            (err
             (message "rexel: forward to %s: %s"
                      (rexel--forward-target forward) (rexel--error-string err))
             (rexel--relay-close relay))
            (t
             (condition-case nil
                 (rexel--relay-connect relay (alist-get 'ticket result))
               (error (rexel--relay-close relay)))))))
      (error (rexel--relay-close relay)))))

(defun rexel--relay-connect (relay ticket)
  "Connect the stream of RELAY to the data endpoint with TICKET."
  (setf (rexel--relay-state relay) 'welcome
        (rexel--relay-ws relay)
        (with-current-buffer (rexel--forward-buffer (rexel--relay-forward relay))
          (rexel--open-data-ws
           "rex.data.bytes.v1" (rexel--hello ticket)
           (lambda (_ws frame)
             (rexel--on-relay-message relay (websocket-frame-payload frame)))
           (lambda (_ws) (rexel--relay-close relay))))))

(defun rexel--on-relay-message (relay bytes)
  "Handle BYTES, one message of the data stream of RELAY.
After the WELCOME every message is for the local connection, and an
empty one says that the target sends no more."
  (pcase (rexel--relay-state relay)
    ('welcome
     (if (eq (rexel--handshake bytes) 'welcome)
         (progn
           (setf (rexel--relay-state relay) 'open)
           (rexel--relay-later relay 'out))
       (rexel--relay-close relay)))
    ('open
     (rexel--relay-queue relay 'in (if (string-empty-p bytes) 'eof bytes)))))

(defun rexel--relay-queue (relay side &rest items)
  "Queue ITEMS for SIDE of RELAY and have that side's queue written.
SIDE is `out' for the stream or `in' for the local connection, and an
item bytes or the symbol `eof'."
  (unless (eq (rexel--relay-state relay) 'closed)
    (if (eq side 'out)
        (progn (cl-callf nconc (rexel--relay-out relay) items)
               (rexel--relay-send relay))
      (cl-callf nconc (rexel--relay-in relay) items)
      (rexel--relay-later relay 'in))))

(defun rexel--relay-later (relay side &optional delay)
  "Write what SIDE of RELAY has queued from a timer, after DELAY seconds.
A message handler must not wait for room in a socket.  websocket.el
decodes the frames of a read and then runs their handlers in turn; the
filter running again inside such a wait would handle later frames before
the rest of these."
  (unless (memq side (rexel--relay-busy relay))
    (push side (rexel--relay-busy relay))
    (run-at-time (or delay 0) nil
                 (lambda ()
                   (setf (rexel--relay-busy relay)
                         (delq side (rexel--relay-busy relay)))
                   (if (eq side 'out)
                       (rexel--relay-send relay)
                     (rexel--relay-deliver relay))))))

(defun rexel--relay-send (relay)
  "Write the next item queued for the stream of RELAY.
Those after it go from a timer each, as an upload's chunks do: a message
written before the server read the last one waits 20 ms for room in a Unix
socket.  At `eof' the local connection is gone, and the stream is ended."
  (when (and (eq (rexel--relay-state relay) 'open)
             (not (memq 'out (rexel--relay-busy relay))))
    (if rexel--writes-overdue
        (rexel--relay-later relay 'out 0.01)
      (when-let* ((item (pop (rexel--relay-out relay))))
        (let ((process (websocket-conn (rexel--relay-ws relay)))
              (client (rexel--relay-client relay)))
          (push 'out (rexel--relay-busy relay))
          (unwind-protect
              (condition-case nil
                  (rexel--watched-write
                   process (lambda () (rexel--relay-close relay))
                   (lambda ()
                     (process-send-string
                      process
                      (if (stringp item)
                          (rexel--binary-frame item)
                        ;; An empty message ends this side of the stream,
                        ;; and a close frame of status 1000 under a zero
                        ;; mask key the stream.
                        (concat (rexel--binary-frame "")
                                (unibyte-string #x88 #x82 0 0 0 0 3 232)))))
                   (if (stringp item) (length item) 0))
                (error (rexel--relay-close relay)))
            (setf (rexel--relay-busy relay)
                  (delq 'out (rexel--relay-busy relay))))
          (when (and (stringp item)
                     (< (cl-decf (rexel--relay-backlog relay) (length item))
                        (/ rexel--relay-backlog-bytes 2))
                     (eq (process-filter client) t)
                     (process-live-p client))
            (set-process-filter client #'rexel--relay-filter))
          (when (rexel--relay-out relay)
            (rexel--relay-later relay 'out (rexel--upload-pause process))))))))

(defun rexel--relay-deliver (relay)
  "Write the items queued for the local connection of RELAY, oldest first.
At `eof' the connection is told that no more follows; it stays open for
what it still sends."
  (when (and (eq (rexel--relay-state relay) 'open)
             (not (memq 'in (rexel--relay-busy relay))))
    (let* ((client (rexel--relay-client relay))
           (stream (websocket-conn (rexel--relay-ws relay)))
           (filter (process-filter stream))
           items)
      (push 'in (rexel--relay-busy relay))
      ;; A connection slower than its target holds the target back instead
      ;; of filling Emacs.
      (set-process-filter stream t)
      (unwind-protect
          (condition-case nil
              ;; What is queued during a write is written after it.
              (while (setq items (cl-shiftf (rexel--relay-in relay) nil))
                (dolist (item items)
                  (when (process-live-p client)
                    (if (eq item 'eof)
                        (process-send-eof client)
                      (rexel--watched-write
                       client (lambda () (rexel--relay-close relay))
                       (lambda () (process-send-string client item))
                       (length item))))))
            (error (rexel--relay-close relay)))
        (when (process-live-p stream)
          (set-process-filter stream filter))
        (setf (rexel--relay-busy relay)
              (delq 'in (rexel--relay-busy relay)))))))

(defun rexel--relay-close (relay)
  "End RELAY: close its local connection and drop its stream."
  (unless (eq (rexel--relay-state relay) 'closed)
    (setf (rexel--relay-state relay) 'closed)
    (let ((forward (rexel--relay-forward relay)))
      (setf (rexel--forward-relays forward)
            (delq relay (rexel--forward-relays forward))))
    (when (process-live-p (rexel--relay-client relay))
      (delete-process (rexel--relay-client relay)))
    (when-let* ((ws (rexel--relay-ws relay)))
      (ignore-errors (delete-process (websocket-conn ws))))))


;;; Listening ports

(defvar-local rexel--ports 'unknown
  "The ports the programs of this block listen on, or `unknown'.
Each is (PORT HOST LABEL), see `rexel--ports-parse'.")

(defun rexel--ports-complete-p (result)
  "Return non-nil if a port missing from RESULT is closed.
RESULT is a reply of the terminal's `ports' method or the payload of its
`ports_changed' event."
  (and (equal (alist-get 'status result) "ok")
       (not (alist-get 'truncated result))))

(defun rexel--ports-parse (result)
  "Return the ports in RESULT as (PORT HOST LABEL)s, lowest port first.
RESULT is as for `rexel--ports-complete-p'.  HOST is where the block's
server reaches PORT: \"localhost\" for a port bound to a loopback address
or to every address, else the address.  LABEL names the process holding
the port, or is nil."
  (let (ports)
    (seq-doseq (entry (alist-get 'ports result))
      (let* ((port (alist-get 'port entry))
             (address (alist-get 'address entry))
             (host (if (member address rexel--loopback-addresses)
                       "localhost"
                     address))
             (known (assq port ports)))
        (when (equal (alist-get 'protocol entry) "tcp")
          (cond ((null known)
                 (push (list port host
                             (rexel--ports-label
                              (alist-get 'pid entry)
                              (alist-get 'processes result)))
                       ports))
                ((equal host "localhost")
                 (setcar (cdr known) host))))))
    (sort ports (lambda (a b) (< (car a) (car b))))))

(defun rexel--ports-label (pid processes)
  "Return a name for the process PID among PROCESSES, or nil.
It is the last part of its script, its argv0 or its name, whichever the
server gives first."
  (when-let* ((process (seq-find (lambda (p) (eql pid (alist-get 'pid p)))
                                 processes))
              (name (or (alist-get 'script process) (alist-get 'argv0 process)
                        (alist-get 'name process))))
    (file-name-nondirectory name)))

(defun rexel--ports-request (callback)
  "Call CALLBACK in this buffer with the server's list of the block's ports.
The list is as for `rexel--ports-complete-p'."
  (let ((buf (current-buffer)))
    (rexel--request
     rexel--conn "com.superlogical.terminal.ports"
     `((session_id . ,rexel--session-id) (block_id . ,rexel--block-id))
     (lambda (result err)
       (cond (err (message "rexel: terminal.ports: %s" (rexel--error-string err)))
             ((buffer-live-p buf)
              (with-current-buffer buf (funcall callback result))))))))

(defun rexel--ports-note (result)
  "Remember the ports in RESULT for this block and return those new in it.
RESULT is as for `rexel--ports-complete-p'.  Nothing is new in the first
list.  A list that may leave out ports forgets none."
  (let* ((ports (rexel--ports-parse result))
         (old rexel--ports)
         (known (listp old)))
    (setq rexel--ports
          (if (or (not known) (rexel--ports-complete-p result))
              ports
            (append ports (seq-remove (lambda (entry) (assq (car entry) ports))
                                      old))))
    (and known
         (seq-remove (lambda (entry) (assq (car entry) old)) ports))))

(defun rexel--ports-update (result)
  "Follow RESULT, the server's list of the ports this block listens on.
Close the forwards of the ports gone from it and open or name those new
in it, as `rexel-port-forward-when-port-closes' and
`rexel-port-forward-new-ports' say.  RESULT is as for
`rexel--ports-complete-p'."
  (when (rexel--conn-host rexel--conn)
    (let* ((old rexel--ports)
           (new (rexel--ports-note result))
           (ports (rexel--ports-parse result))
           named)
      (when (and (listp old) (rexel--ports-complete-p result)
                 (not (eq rexel-port-forward-when-port-closes 'keep)))
        (dolist (forward (rexel--buffer-forwards (current-buffer)))
          (let ((port (rexel--forward-port forward))
                (host (rexel--forward-host forward)))
            (when (and (equal host (nth 1 (assq port old)))
                       (not (equal host (nth 1 (assq port ports)))))
              (when (eq rexel-port-forward-when-port-closes 'remember)
                (setf (alist-get port rexel--forward-remembered)
                      (rexel--forward-local-port forward)))
              (rexel--forward-close forward)
              (message "rexel: %s: port %d closed, and its forward"
                       (buffer-name) port)))))
      (pcase-dolist (`(,port ,host ,label) ports)
        (let ((local (alist-get port rexel--forward-remembered)))
          (cond
           ((rexel--forward-find rexel--conn host port))
           ((or local (and (assq port new)
                           (eq rexel-port-forward-new-ports t)))
            (setf (alist-get port rexel--forward-remembered nil t) nil)
            (message "rexel: %s"
                     (rexel--forward-describe
                      (rexel--forward-open host port local))))
           ((and (assq port new) (eq rexel-port-forward-new-ports 'message))
            (push (format "%s listens on port %d" (or label "a program") port)
                  named)))))
      (when named
        (message "rexel: %s: %s (%s forwards)" (buffer-name)
                 (string-join (nreverse named) ", ")
                 (substitute-command-keys "\\[rexel-port-forward]"))))))

(defun rexel--ports-fetch ()
  "Ask the server for this block's ports and follow its answer."
  (rexel--ports-request #'rexel--ports-update))


;;; Links

(defconst rexel--forward-handler
  (cons (concat "\\`https?://\\(?:localhost\\|127\\.0\\.0\\.1\\|0\\.0\\.0\\.0"
                "\\|\\[::1?\\]\\)\\(?:[:/?#]\\|\\'\\)")
        #'rexel--forward-browse)
  "The entry of `browse-url-handlers' for links to localhost in block buffers.")

(defun rexel--forward-browse (url &rest args)
  "Open URL, a link to localhost, with ARGS as `browse-url' does.
In a block buffer of a server on another machine, forward the link's port
first and open the link on the local port; see `rexel-port-forward-links'."
  (when (and rexel-port-forward-links
             (ignore-errors (rexel--forward-ensure-context) t))
    (let* ((parsed (url-generic-parse-url url))
           (forward (rexel--forward-open "localhost" (url-port parsed))))
      (unless (member (url-host parsed) '("localhost" "127.0.0.1"))
        (setf (url-host parsed) "127.0.0.1"))
      (setf (url-portspec parsed) (rexel--forward-local-port forward))
      (setq url (url-recreate-url parsed))
      (message "rexel: %s" (rexel--forward-describe forward))))
  (let ((browse-url-handlers (remq rexel--forward-handler browse-url-handlers)))
    (apply #'browse-url url args)))

(defun rexel--forward-setup ()
  "Prepare this block buffer for forwards: its links and its list of ports.
A connection lost meanwhile asks for the ports when it is back."
  (setq-local browse-url-handlers
              (cons rexel--forward-handler
                    (remq rexel--forward-handler browse-url-handlers)))
  (when (and (rexel--conn-host rexel--conn) (rexel--ready-p rexel--conn))
    (rexel--ports-fetch)))


;;; Commands

(defun rexel--forward-parse-target (string)
  "Return (HOST . PORT) for STRING, a port, a HOST:PORT or a URL, or nil.
A port alone, and a name or address of the machine itself, give the host
\"localhost\"."
  (let* ((url (and (string-search "://" string) (url-generic-parse-url string)))
         (host (cond (url (url-host url))
                     ((string-match "\\`\\(.+\\):[0-9]+\\'" string)
                      (match-string 1 string))
                     (t "localhost")))
         (port (cond (url (url-port url))
                     ((string-match "\\(?:\\`\\|:\\)\\([0-9]+\\)\\'" string)
                      (string-to-number (match-string 1 string)))))
         (host (and host (string-trim host "\\[" "\\]"))))
    (when (and (integerp port) (<= 1 port 65535)
               host (not (string-empty-p host)))
      (cons (if (member host rexel--loopback-addresses) "localhost" host)
            port))))

(defun rexel--forward-read-target ()
  "Read the target of a forward on this block's server; return (HOST . PORT)."
  (let ((string (string-trim
                 (read-string (format "Forward port, host:port or URL on %s: "
                                      (rexel--conn-name rexel--conn))))))
    (or (rexel--forward-parse-target string)
        (user-error "Not a port, a host:port or a URL: %s" string))))

(defun rexel--forward-show (forward)
  "Say what FORWARD forwards and put its URL in the kill ring."
  (kill-new (rexel--forward-url forward))
  (message "rexel: %s" (rexel--forward-describe forward)))

(defun rexel--forward-choose (ports)
  "Read one of PORTS, each a (PORT HOST LABEL), and return its (HOST . PORT)."
  (let ((choices (mapcar
                  (pcase-lambda (`(,port ,host ,label))
                    (cons (concat (number-to-string port)
                                  (and label (concat " " label))
                                  (and (rexel--forward-find rexel--conn host port)
                                       " (forwarded)"))
                          (cons host port)))
                  ports)))
    (or (cdr (assoc (completing-read "Forward port: " choices nil t) choices))
        (user-error "No port chosen"))))

(defun rexel-port-forward (&optional port host)
  "Forward PORT of the current block's machine to a port of this one.
HOST is another host the block's server reaches PORT on.  Without PORT,
forward the port the block's programs listen on; read one of them when
there are several, and a port, a HOST:PORT or a URL when there is none.
Interactively, a prefix argument reads one of those in any case.

The forward listens on the same port of this machine, or on another one when
that is taken.  Its URL is shown and put in the kill ring.
It ends with `rexel-port-forward-close', with its block, and as
`rexel-port-forward-when-port-closes' says when the block stops
listening on the port."
  (interactive
   (progn
     (rexel--forward-ensure-context)
     (and current-prefix-arg
          (let ((target (rexel--forward-read-target)))
            (list (cdr target) (car target))))))
  (rexel--forward-ensure-context)
  (if port
      (rexel--forward-show (rexel--forward-open (or host "localhost") port))
    (rexel--ports-request
     (lambda (result)
       (rexel--ports-note result)
       (let ((ports (rexel--ports-parse result))
             (buf (current-buffer)))
         (if (and ports (null (cdr ports)))
             (rexel--forward-show
              (rexel--forward-open (nth 1 (car ports)) (caar ports)))
           (rexel--prompt-later
            (lambda ()
              (when (buffer-live-p buf)
                (with-current-buffer buf
                  (condition-case err
                      (pcase-let ((`(,host . ,port)
                                   (if ports
                                       (rexel--forward-choose ports)
                                     (rexel--forward-read-target))))
                        (rexel--forward-ensure-context)
                        (rexel--forward-show (rexel--forward-open host port)))
                    (user-error
                     (message "%s" (error-message-string err))))))))))))))

(defun rexel--forward-ensure-context ()
  "Signal an error unless this buffer's block can have a port forwarded."
  (rexel--ensure-block-buffer-context)
  (unless (rexel--conn-host rexel--conn)
    (user-error "This block runs on this machine; its ports need no forward"))
  (when (member rexel--session-id (rexel--conn-attaching rexel--conn))
    (user-error "The session is still being reattached")))

(defun rexel-port-forward-close (forward)
  "Close FORWARD, a forward of `rexel-port-forward'.
Interactively, read one of the open forwards.
Only one is closed without asking."
  (interactive
   (list (rexel--choose
          "Close forward: "
          (or (mapcar (lambda (forward)
                        (cons (rexel--forward-describe forward) forward))
                      rexel--forwards)
              (user-error "No port is forwarded")))))
  (rexel--forward-close forward)
  (message "rexel: closed the forward to %s on %s"
           (rexel--forward-target forward)
           (rexel--conn-name (rexel--forward-conn forward))))

(provide 'rexel-port-forward)
;;; rexel-port-forward.el ends here
