;;; rexel-port-forward-test.el --- Tests for rexel-port-forward -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests of the port forwards with the server's connections stubbed, on
;; the helpers of rexel-test.el.  The local side is real: a forward
;; listens on a port of 127.0.0.1 and the tests connect to it.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'rexel)
(require 'rexel-test)

(defvar rexel-test--opened nil
  "The data connections a test opened, see `rexel-test--with-forwards'.")

(defvar rexel-test--frames nil
  "The frames a test sent on data connections, see `rexel-test--with-forwards'.")

(defmacro rexel-test--with-forwards (sent buf &rest body)
  "Run BODY in BUF, a block buffer of a fake connection to the host \"studio\".
The requests collect in SENT, a (WS ON-OPEN ON-MESSAGE ON-CLOSE) per
data connection in `rexel-test--opened' and the (WS OPCODE PAYLOAD) of
each frame sent on one in `rexel-test--frames', all newest first; a close
frame's payload is its status."
  (declare (indent 2))
  `(let ((rexel--forwards nil)
         (send (symbol-function 'process-send-string))
         (rexel-test--opened nil)
         (rexel-test--frames nil))
     (rexel-test--with-block-buffer ,sent ,buf
       (setf (rexel--conn-host rexel--conn) "studio")
       (unwind-protect
           (cl-letf (((symbol-function 'rexel--ws-open)
                      (lambda (_endpoint _path _protocol &rest callbacks)
                        (let ((ws (rexel-test--ws)))
                          (push (cons ws callbacks) rexel-test--opened)
                          ws)))
                     ((symbol-function 'websocket-send)
                      (lambda (ws frame)
                        (push (list ws (websocket-frame-opcode frame)
                                    (websocket-frame-payload frame))
                              rexel-test--frames)))
                     ;; A relay writes its rexel-test--frames itself.
                     ((symbol-function 'process-send-string)
                      (lambda (process string)
                        (if-let* ((ws (process-get process :websocket)))
                            (while (not (string-empty-p string))
                              (let* ((frame (websocket-read-frame string))
                                     (close (eq (websocket-frame-opcode frame) 'close)))
                                ;; websocket.el reads no status from a
                                ;; close frame, which ends a write.
                                (push (list ws (websocket-frame-opcode frame)
                                            (if close
                                                (substring string -2)
                                              (websocket-frame-payload frame)))
                                      rexel-test--frames)
                                (setq string
                                      (if close
                                          ""
                                        (substring
                                         string (websocket-frame-length frame))))))
                          (funcall send process string)))))
             (with-current-buffer ,buf
               ,@body))
         (dolist (forward rexel--forwards)
           (ignore-errors (delete-process (rexel--forward-listener forward)))
           (dolist (relay (rexel--forward-relays forward))
             (ignore-errors (delete-process (rexel--relay-client relay)))))
         (dolist (open rexel-test--opened)
           (ignore-errors (rexel-test--ws-delete (car open))))))))

(defun rexel-test--until (pred)
  "Read process output and run timers until PRED holds; fail after 3 seconds."
  (let ((end (+ (float-time) 3)))
    (while (and (not (funcall pred)) (< (float-time) end))
      (accept-process-output nil 0.01))
    (should (funcall pred))))

(defun rexel-test--client (port)
  "Connect to PORT of this machine and return the process.
What it receives collects under `:got', and `:closed' is set at its end."
  (make-network-process
   :name "rexel-test-client" :host "127.0.0.1" :service port :family 'ipv4
   :coding 'binary :noquery t
   :filter (lambda (process string)
             (process-put process :got (concat (process-get process :got) string)))
   :sentinel (lambda (process _change)
               (unless (process-live-p process)
                 (process-put process :closed t)))))

(defun rexel-test--tunnel (conn sent &optional bid)
  "Answer the newest `session.new_block' in SENT on CONN with the block BID."
  (rexel-test--respond conn (rexel-test--find "session.new_block" sent)
                       `((block_id . ,(or bid "block:tunnel")) (revision . 1))))

(defun rexel-test--relay-welcome (conn sent)
  "Give the newest `data.connect' in SENT on CONN a ticket and welcome its HELLO.
Return the entry of `rexel-test--opened' for the connection this opens."
  (let ((count (length rexel-test--opened)))
    (rexel-test--respond
     conn (rexel-test--find "data.connect" sent)
     `((ticket . ,(base64url-encode-string (make-string 16 7) t))
       (expires_in_ms . 60000)))
    (should (= (1+ count) (length rexel-test--opened)))
    (pcase-let ((`(,ws ,on-open ,on-message ,_) (car rexel-test--opened)))
      (funcall on-open ws)
      (funcall on-message ws
               (make-websocket-frame
                :opcode 'binary :completep t
                :payload (concat (unibyte-string #x81) (make-string 31 0)))))
    (car rexel-test--opened)))

(defun rexel-test--relay-message (open payload)
  "Hand PAYLOAD to the data connection OPEN as a message of its stream."
  (pcase-let ((`(,ws ,_ ,on-message ,_) open))
    (funcall on-message ws (make-websocket-frame :opcode 'binary :completep t
                                                 :payload payload))))

(defun rexel-test--sent-on (ws)
  "Return the (OPCODE PAYLOAD) of the frames sent on WS, oldest first."
  (let (sent)
    (pcase-dolist (`(,to ,opcode ,payload) rexel-test--frames)
      (when (eq to ws)
        (push (list opcode payload) sent)))
    sent))

(defun rexel-test--closed-block (sent)
  "Return the ID of the block the newest `block.close' in SENT closes."
  (alist-get 'block_id (alist-get 'payload (rexel-test--find "block.close" sent))))

(defun rexel-test--ports (&rest ports)
  "Return a complete `ports' reply listing PORTS, each (PORT ADDRESS SCRIPT)."
  `((status . "ok")
    (ports . ,(vconcat
               (mapcar (pcase-lambda (`(,port ,address ,_))
                         `((port . ,port) (address . ,(or address "127.0.0.1"))
                           (protocol . "tcp") (pid . ,port)))
                       ports)))
    (processes . ,(vconcat
                   (mapcar (pcase-lambda (`(,port ,_ ,script))
                             `((pid . ,port) (name . "node")
                               (script . ,(concat "/srv/bin/" (or script "vite")))))
                           ports)))))

(defun rexel-test--free-port ()
  "Return a port of this machine that nothing listens on."
  (let* ((server (make-network-process :name "rexel-test-port" :server t
                                       :host "127.0.0.1" :service t :family 'ipv4
                                       :noquery t))
         (port (process-contact server :service)))
    (delete-process server)
    port))


;;; Ports

(ert-deftest rexel-test-ports-parse ()
  "A port is listed once, under the host its server reaches it at."
  (let ((result '((status . "ok")
                  (ports . [((port . 5173) (address . "::1") (protocol . "tcp") (pid . 7))
                            ((port . 5173) (address . "127.0.0.1") (protocol . "tcp") (pid . 7))
                            ((port . 3000) (address . "::") (protocol . "tcp") (pid . 8))
                            ((port . 8000) (address . "10.0.0.5") (protocol . "tcp"))
                            ((port . 53) (address . "0.0.0.0") (protocol . "udp") (pid . 8))])
                  (processes . [((pid . 7) (name . "node") (argv0 . "node")
                                 (script . "/app/node_modules/.bin/vite"))
                                ((pid . 8) (name . "ruby") (argv0 . "/usr/bin/ruby"))]))))
    (should (equal '((3000 "localhost" "ruby") (5173 "localhost" "vite")
                     (8000 "10.0.0.5" nil))
                   (rexel--ports-parse result)))
    (should (rexel--ports-complete-p result))
    (should-not (rexel--ports-complete-p '((status . "partial") (ports . []))))
    (should-not (rexel--ports-complete-p '((status . "ok") (truncated . t))))))

(ert-deftest rexel-test-ports-note-names-the-new-ones ()
  "Nothing is new in a block's first list, and a partial list forgets no port."
  (with-temp-buffer
    (should-not (rexel--ports-note (rexel-test--ports '(5173))))
    (should (equal '((3000 "localhost" "vite"))
                   (rexel--ports-note (rexel-test--ports '(3000) '(5173)))))
    (should-not (rexel--ports-note '((status . "partial") (ports . []))))
    (should (equal '(3000 5173) (mapcar #'car rexel--ports)))
    (should-not (rexel--ports-note (rexel-test--ports '(3000))))
    (should (equal '(3000) (mapcar #'car rexel--ports)))))

(ert-deftest rexel-test-forward-parse-target ()
  (should (equal '("localhost" . 5173) (rexel--forward-parse-target "5173")))
  (should (equal '("db" . 5432) (rexel--forward-parse-target "db:5432")))
  (should (equal '("localhost" . 80) (rexel--forward-parse-target "[::1]:80")))
  (should (equal '("localhost" . 3000)
                 (rexel--forward-parse-target "http://127.0.0.1:3000/app")))
  (should (equal '("db.internal" . 443)
                 (rexel--forward-parse-target "https://db.internal/x")))
  (dolist (bad '("" "vite" "db:" "0" "70000" "db:99999"))
    (should-not (rexel--forward-parse-target bad))))


;;; Forwards

(ert-deftest rexel-test-forward-open-makes-a-tunnel-block ()
  "A forward listens at once and asks for a detached dial tunnel."
  (rexel-test--with-forwards sent buf
    (let* ((port (rexel-test--free-port))
           (forward (rexel--forward-open "localhost" port))
           (payload (alist-get 'payload (car sent))))
      (should (= 1 (length sent)))
      (should (equal "session.new_block" (alist-get 'method (car sent))))
      (should (equal "k" (alist-get 'session_id payload)))
      (should (equal "com.superlogical.tunnel.dial" (alist-get 'flavor payload)))
      (should (equal `((host . "localhost") (port . ,port)
                       (client_address . ,(format "127.0.0.1:%d" port)))
                     (alist-get 'options payload)))
      (should (equal (format "rexel %d:localhost:%d" port port)
                     (alist-get 'label payload)))
      (should (= port (rexel--forward-local-port forward)))
      (should (process-live-p (rexel--forward-listener forward)))
      (should-not (rexel--forward-block forward))
      ;; The target has one forward.
      (should (eq forward (rexel--forward-open "localhost" port)))
      (should (= 1 (length sent)))
      (rexel-test--tunnel rexel--conn sent)
      (should (equal "block:tunnel" (rexel--forward-block forward))))))

(ert-deftest rexel-test-forward-takes-another-port-when-its-own-is-taken ()
  (rexel-test--with-forwards sent buf
    (let* ((holder (make-network-process :name "rexel-test-holder" :server t
                                         :host "127.0.0.1" :service t
                                         :family 'ipv4 :noquery t))
           (port (process-contact holder :service)))
      (unwind-protect
          (let ((forward (rexel--forward-open "localhost" port)))
            (should-not (= port (rexel--forward-local-port forward)))
            (should (equal port (alist-get 'port (alist-get 'options
                                                             (alist-get 'payload (car sent)))))))
        (delete-process holder)))))

(defun rexel-test--holder (address family port)
  "Return a process listening on PORT of ADDRESS in FAMILY, or nil."
  (ignore-errors
    (make-network-process :name "rexel-test-holder" :server t :noquery t
                          :host address :family family :service port)))

(ert-deftest rexel-test-forward-port-held-on-127.0.0.1-is-taken ()
  (let* ((port (rexel-test--free-port))
         (holder (rexel-test--holder "127.0.0.1" 'ipv4 port)))
    (unwind-protect
        (should-not (rexel--forward-port-free-p port))
      (delete-process holder))
    (should (rexel--forward-port-free-p port))))

(ert-deftest rexel-test-forward-port-held-on-::1-is-taken ()
  "A port a program holds on ::1 alone is not free: localhost reaches it there."
  (let* ((port (rexel-test--free-port))
         (holder (rexel-test--holder "::1" 'ipv6 port)))
    (skip-unless holder)
    (unwind-protect
        (should-not (rexel--forward-port-free-p port))
      (delete-process holder))))

(ert-deftest rexel-test-forward-port-of-a-forward-is-taken-unasked ()
  "The port of a forward counts as taken without a connection to it."
  (rexel-test--with-forwards sent buf
    (let ((forward (rexel--forward-open "localhost" (rexel-test--free-port))))
      (rexel-test--tunnel rexel--conn sent)
      (should-not (rexel--forward-port-free-p (rexel--forward-local-port forward)))
      (dotimes (_ 5) (accept-process-output nil 0.01))
      (should-not (rexel--forward-relays forward))
      (should-not (rexel-test--find "data.connect" sent)))))

(ert-deftest rexel-test-forward-refused-by-the-server-closes ()
  (rexel-test--with-forwards sent buf
    (let ((forward (rexel--forward-open "localhost" (rexel-test--free-port))))
      (rexel-test--respond rexel--conn (car sent) nil
                           '((code . "conflict") (message . "not attached")))
      (should-not rexel--forwards)
      (should-not (process-live-p (rexel--forward-listener forward)))
      (should-not (rexel-test--find "block.close" sent)))))

(ert-deftest rexel-test-forward-close-closes-its-block ()
  (rexel-test--with-forwards sent buf
    (let ((forward (rexel--forward-open "localhost" (rexel-test--free-port)))
          done)
      (rexel-test--tunnel rexel--conn sent)
      (rexel--forward-close forward (lambda () (setq done t)))
      (should-not rexel--forwards)
      (should-not (process-live-p (rexel--forward-listener forward)))
      (should (equal '((session_id . "k") (block_id . "block:tunnel"))
                     (alist-get 'payload (rexel-test--find "block.close" sent))))
      (should-not done)
      (rexel-test--respond rexel--conn (rexel-test--find "block.close" sent)
                           '((block_id . "block:tunnel")))
      (should done))))

(ert-deftest rexel-test-forward-closed-before-its-block-closes-it-later ()
  "A forward closed while its block is being made closes the block it gets."
  (rexel-test--with-forwards sent buf
    (let ((forward (rexel--forward-open "localhost" (rexel-test--free-port)))
          done)
      (rexel--forward-close forward (lambda () (setq done t)))
      (should-not (rexel-test--find "block.close" sent))
      (should-not done)
      (rexel-test--tunnel rexel--conn sent)
      (should (equal "block:tunnel"
                     (rexel-test--closed-block sent)))
      (should-not done)
      (rexel-test--respond rexel--conn (rexel-test--find "block.close" sent)
                           '((block_id . "block:tunnel")))
      (should done))))

(ert-deftest rexel-test-forward-keeps-the-block-of-its-latest-request ()
  "A block that arrives after a later request for one is closed."
  (rexel-test--with-forwards sent buf
    (let* ((conn rexel--conn)
           (forward (rexel--forward-open "localhost" (rexel-test--free-port)))
           (first (car sent)))
      ;; A redial asks again while the first answer is on its way.
      (rexel--forwards-resume conn "k")
      (should (= 2 (rexel-test--count "session.new_block" sent)))
      (rexel-test--respond conn first '((block_id . "block:first")))
      (should-not (rexel--forward-block forward))
      (should (equal "block:first"
                     (rexel-test--closed-block sent)))
      (rexel-test--tunnel conn sent "block:second")
      (should (equal "block:second" (rexel--forward-block forward)))
      (should (= 1 (rexel-test--count "block.close" sent))))))

(ert-deftest rexel-test-forward-block-closed-on-the-server-closes-forward ()
  (rexel-test--with-forwards sent buf
    (let ((forward (rexel--forward-open "localhost" (rexel-test--free-port))))
      (rexel-test--tunnel rexel--conn sent)
      (rexel--on-event rexel--conn (rexel-test--block-closed "k" "block:tunnel"))
      (should-not rexel--forwards)
      (should-not (process-live-p (rexel--forward-listener forward)))
      (should-not (rexel-test--find "block.close" sent))
      (should (buffer-live-p buf)))))

(ert-deftest rexel-test-killed-buffer-closes-its-forwards ()
  (rexel-test--with-forwards sent buf
    (rexel--forward-open "localhost" (rexel-test--free-port))
    (rexel-test--tunnel rexel--conn sent)
    (let ((rexel--keep-blocks t))
      (kill-buffer buf))
    (should-not rexel--forwards)
    (should (equal "block:tunnel"
                   (rexel-test--closed-block sent)))))

(ert-deftest rexel-test-detach-waits-for-its-forwards-to-close ()
  "The session is detached once the server closed its tunnel blocks."
  (rexel-test--with-forwards sent buf
    (let ((conn rexel--conn))
      (rexel--forward-open "localhost" (rexel-test--free-port))
      (rexel-test--tunnel conn sent)
      (rexel-detach)
      (should-not (buffer-live-p buf))
      (should-not rexel--forwards)
      (should-not (rexel-test--find "session.detach" sent))
      (rexel-test--respond conn (rexel-test--find "block.close" sent)
                           '((block_id . "block:tunnel")))
      (should (equal '((session_id . "k"))
                     (alist-get 'payload (rexel-test--find "session.detach" sent))))))
  ;; So it is with a tunnel block still being made.
  (rexel-test--with-forwards sent buf
    (let ((conn rexel--conn))
      (rexel--forward-open "localhost" (rexel-test--free-port))
      (rexel-detach)
      (should-not (rexel-test--find "session.detach" sent))
      (rexel-test--tunnel conn sent)
      (should-not (rexel-test--find "session.detach" sent))
      (rexel-test--respond conn (rexel-test--find "block.close" sent)
                           '((block_id . "block:tunnel")))
      (should (rexel-test--find "session.detach" sent))))
  ;; Without a forward it is detached at once.
  (rexel-test--with-forwards sent buf
    (rexel-detach)
    (should (equal "session.detach" (alist-get 'method (car sent))))))

(ert-deftest rexel-test-forwards-leave-closes-blocks-and-returns ()
  (rexel-test--with-forwards sent buf
    (let ((rexel--exit-timeout 0.05))
      (rexel--forward-open "localhost" (rexel-test--free-port))
      (rexel-test--tunnel rexel--conn sent)
      (rexel--forwards-leave)
      (should-not rexel--forwards)
      (should (rexel-test--find "block.close" sent)))))

(ert-deftest rexel-test-forwards-resume-takes-blocks-back ()
  "After a redial a tunnel is taken back, and made again when it is gone."
  (rexel-test--with-forwards sent buf
    (let ((conn rexel--conn)
          (forward (rexel--forward-open "localhost" (rexel-test--free-port))))
      (rexel-test--tunnel conn sent)
      (setq sent nil)
      (rexel--forwards-resume conn "k")
      (should (equal '((session_id . "k") (block_ids . ["block:tunnel"])
                       (mode . "take"))
                     (alist-get 'payload (car sent))))
      (rexel-test--respond conn (car sent) '((blocks . [])))
      (should-not (rexel-test--find "session.new_block" sent))
      (rexel--forwards-resume conn "k")
      (rexel-test--respond conn (car sent) nil
                           '((code . "not_found") (message . "no such block")))
      (should-not (rexel--forward-block forward))
      ;; The server may still have the block that could not be taken.
      (should (equal "block:tunnel"
                     (rexel-test--closed-block sent)))
      (rexel-test--tunnel conn sent "block:again")
      (should (equal "block:again" (rexel--forward-block forward))))))

(ert-deftest rexel-test-reattach-resumes-forwards-and-closes-leftovers ()
  "A redial takes the tunnels back and looks for those left without an owner."
  (rexel-test--with-forwards sent buf
    (let ((conn rexel--conn))
      (rexel--forward-open "localhost" (rexel-test--free-port))
      (rexel-test--tunnel conn sent)
      (setq sent nil)
      (cl-letf (((symbol-function 'rexel--apply-view) #'ignore)
                ((symbol-function 'rexel--sync-theme) #'ignore)
                ((symbol-function 'rexel--sync-size) #'ignore))
        (rexel--reattach conn)
        (rexel-test--respond conn (rexel-test--find "session.attach" sent)
                             '((view . nil))))
      (should (rexel-test--find "session.own_block" sent))
      (should (rexel-test--find "session.list_blocks" sent))
      (should (rexel-test--find "com.superlogical.terminal.ports" sent)))))

(ert-deftest rexel-test-reattach-refused-closes-the-sessions-forwards ()
  "A session that cannot be attached again keeps no forward."
  (rexel-test--with-forwards sent buf
    (let* ((conn rexel--conn)
           (inhibit-message t)
           (forward (rexel--forward-open "localhost" (rexel-test--free-port))))
      (rexel-test--tunnel conn sent)
      (setq sent nil)
      (rexel--reattach conn)
      (rexel-test--respond conn (rexel-test--find "session.attach" sent) nil
                           '((code . "conflict") (message . "busy")))
      (should-not rexel--forwards)
      (should-not (process-live-p (rexel--forward-listener forward))))))

(ert-deftest rexel-test-forward-leftovers-are-closed ()
  "Tunnel blocks of rexel without an owner are closed, others left alone."
  (rexel-test--with-forwards sent buf
    (let ((conn rexel--conn)
          (tunnel "com.superlogical.tunnel"))
      (rexel--forward-open "localhost" (rexel-test--free-port))
      (rexel-test--tunnel conn sent "block:mine")
      (rexel--forward-close-leftovers conn "k")
      (should (equal "session.list_blocks" (alist-get 'method (car sent))))
      (rexel-test--respond
       conn (car sent)
       `((blocks . [((block_id . "block:shell") (label . "rexel shell")
                     (creator_name . "com.superlogical.terminal"))
                    ((block_id . "block:left") (label . "rexel 5173:localhost:5173")
                     (creator_name . ,tunnel))
                    ((block_id . "block:owned") (label . "rexel 80:localhost:80")
                     (creator_name . ,tunnel) (owner . "client:other"))
                    ((block_id . "block:cli") (label . "fwd -L 1:localhost:1")
                     (creator_name . ,tunnel))
                    ((block_id . "block:mine") (label . "rexel 1:localhost:1")
                     (creator_name . ,tunnel))])))
      (should (equal '("block:left")
                     (delq nil
                           (mapcar (lambda (object)
                                     (and (equal "block.close" (alist-get 'method object))
                                          (alist-get 'block_id
                                                     (alist-get 'payload object))))
                                   sent))))
      ;; This machine's server has none.
      (setf (rexel--conn-host conn) nil)
      (setq sent nil)
      (rexel--forward-close-leftovers conn "k")
      (should-not sent))))


;;; Relays

(ert-deftest rexel-test-relay-carries-a-connection-both-ways ()
  "A connection to the local port gets a data stream of the tunnel block."
  (rexel-test--with-forwards sent buf
    (let* ((conn rexel--conn)
           (forward (rexel--forward-open "localhost" (rexel-test--free-port)))
           (client (rexel-test--client (rexel--forward-local-port forward))))
      (process-send-string client "early ")
      (rexel-test--until (lambda () (rexel--forward-relays forward)))
      ;; Nothing is asked for before the tunnel block exists.
      (should-not (rexel-test--find "data.connect" sent))
      (rexel-test--tunnel conn sent)
      (let ((payload (alist-get 'payload (rexel-test--find "data.connect" sent)))
            (case-fold-search nil))
        (should (equal "block:tunnel" (alist-get 'block_id payload)))
        (should (equal "rex.data.bytes.v1" (alist-get 'subprotocol payload)))
        (should (string-match-p "\\`conn:[0-9a-f-]+\\'"
                                (alist-get 'data_ref_id payload))))
      (let* ((open (rexel-test--relay-welcome conn sent))
             (ws (car open)))
        (should (equal (list 'binary (concat "\x01" (make-string 16 7)))
                       (car (rexel-test--sent-on ws))))
        (process-send-string client "request")
        (rexel-test--until
         (lambda ()
           (equal "early request"
                  (mapconcat #'cadr (cdr (rexel-test--sent-on ws)) ""))))
        ;; What the target sends, then its end.
        (rexel-test--relay-message open "re")
        (rexel-test--relay-message open "ply")
        (rexel-test--relay-message open "")
        (should-not (process-get client :got))
        (rexel-test--until (lambda () (process-get client :closed)))
        (should (equal "reply" (process-get client :got)))
        ;; The local connection is gone: its end, and the stream's.
        (rexel-test--until
         (lambda ()
           (equal '((binary "") (close "\x03\xe8"))
                  (last (rexel-test--sent-on ws) 2))))
        (funcall (nth 3 open) ws)
        (should-not (rexel--forward-relays forward))))))

(ert-deftest rexel-test-relay-sends-in-small-messages ()
  "Bytes go to the stream in order, a socket buffer's worth per message."
  (rexel-test--with-forwards sent buf
    (let* ((conn rexel--conn)
           (forward (rexel--forward-open "localhost" (rexel-test--free-port)))
           (client (rexel-test--client (rexel--forward-local-port forward)))
           (bytes (apply #'unibyte-string
                         (mapcar (lambda (i) (% i 251)) (number-sequence 1 10000)))))
      (rexel-test--tunnel conn sent)
      (rexel-test--until (lambda () (rexel-test--find "data.connect" sent)))
      (let ((ws (car (rexel-test--relay-welcome conn sent))))
        (process-send-string client bytes)
        (rexel-test--until
         (lambda ()
           (equal bytes
                  (mapconcat #'cadr (cdr (rexel-test--sent-on ws)) ""))))
        (should (seq-every-p (lambda (frame)
                               (<= (length (cadr frame)) rexel--upload-chunk-bytes))
                             (cdr (rexel-test--sent-on ws))))))))

(ert-deftest rexel-test-relay-of-a-connection-closed-early-still-delivers ()
  "What a connection sent before it closed reaches the target, then its end."
  (rexel-test--with-forwards sent buf
    (let* ((conn rexel--conn)
           (forward (rexel--forward-open "localhost" (rexel-test--free-port)))
           (client (rexel-test--client (rexel--forward-local-port forward))))
      (process-send-string client "one shot")
      (rexel-test--until (lambda () (rexel--forward-relays forward)))
      (rexel-test--until
       (lambda () (rexel--relay-out (car (rexel--forward-relays forward)))))
      (delete-process client)
      (rexel-test--until
       (lambda ()
         (memq 'eof (rexel--relay-out (car (rexel--forward-relays forward))))))
      (rexel-test--tunnel conn sent)
      (let ((ws (car (rexel-test--relay-welcome conn sent))))
        (rexel-test--until
         (lambda ()
           (equal '((binary "one shot") (binary "") (close "\x03\xe8"))
                  (cdr (rexel-test--sent-on ws)))))))))

(ert-deftest rexel-test-relay-failures-close-the-local-connection ()
  "A refused ticket, a refused HELLO and a lost stream end the connection."
  (dolist (failure '(ticket hello close))
    (rexel-test--with-forwards sent buf
      (let* ((conn rexel--conn)
             (forward (rexel--forward-open "localhost" (rexel-test--free-port)))
             (client (rexel-test--client (rexel--forward-local-port forward))))
        (rexel-test--tunnel conn sent)
        (rexel-test--until (lambda () (rexel-test--find "data.connect" sent)))
        (pcase failure
          ('ticket
           (rexel-test--respond conn (rexel-test--find "data.connect" sent) nil
                                '((code . "not_found") (message . "gone"))))
          ('hello
           (rexel-test--respond conn (rexel-test--find "data.connect" sent)
                                '((ticket . "AAAA")))
           (rexel-test--relay-message (car rexel-test--opened) (unibyte-string #x82 3)))
          ('close
           (let ((open (rexel-test--relay-welcome conn sent)))
             (funcall (nth 3 open) (car open)))))
        (rexel-test--until (lambda () (process-get client :closed)))
        (should-not (rexel--forward-relays forward))
        (should (memq forward rexel--forwards))))))

(ert-deftest rexel-test-relay-asks-again-after-a-redial ()
  "A relay whose ticket request went with the connection asks again."
  (rexel-test--with-forwards sent buf
    (let* ((conn rexel--conn)
           (forward (rexel--forward-open "localhost" (rexel-test--free-port)))
           (client (rexel-test--client (rexel--forward-local-port forward))))
      (rexel-test--tunnel conn sent)
      (rexel-test--until (lambda () (rexel-test--find "data.connect" sent)))
      (clrhash (rexel--conn-pending conn))
      (setq sent nil)
      (rexel--forwards-resume conn "k")
      (should-not (rexel-test--find "data.connect" sent))
      (rexel-test--respond conn (rexel-test--find "session.own_block" sent)
                           '((blocks . [])))
      (should (rexel-test--find "data.connect" sent))
      (should (process-live-p client)))))

(ert-deftest rexel-test-relay-waits-while-no-stream-can-open ()
  "A connection accepted during a redial waits until the tunnel is taken back."
  (rexel-test--with-forwards sent buf
    (let* ((conn rexel--conn)
           (forward (rexel--forward-open "localhost" (rexel-test--free-port)))
           clients)
      (rexel-test--tunnel conn sent)
      (setq sent nil)
      (cl-flet ((connect ()
                  (let ((count (length (rexel--forward-relays forward))))
                    (push (rexel-test--client (rexel--forward-local-port forward))
                          clients)
                    (rexel-test--until
                     (lambda () (< count (length (rexel--forward-relays forward))))))))
        ;; The connection is down, then up and attaching, then taking.
        (setf (rexel--conn-client-id conn) nil)
        (connect)
        (setf (rexel--conn-client-id conn) t
              (rexel--conn-attaching conn) (list "k"))
        (connect)
        (setf (rexel--conn-attaching conn) nil)
        (rexel--forwards-resume conn "k")
        (connect)
        (should-not (rexel-test--find "data.connect" sent))
        (rexel-test--respond conn (rexel-test--find "session.own_block" sent)
                             '((blocks . [])))
        (should (= 3 (rexel-test--count "data.connect" sent)))
        (should (seq-every-p #'process-live-p clients))))))

(ert-deftest rexel-test-relay-of-a-closed-forward-is-refused ()
  "A connection accepted as its forward closes is closed, not kept waiting."
  (rexel-test--with-forwards sent buf
    (let* ((forward (rexel--forward-open "localhost" (rexel-test--free-port)))
           (client (make-pipe-process :name "rexel-test-late" :noquery t)))
      (process-put client :forward forward)
      (rexel--forward-close forward)
      (should-not (rexel--relay-of client))
      (should-not (process-live-p client))
      (should-not (rexel--forward-relays forward)))))

(ert-deftest rexel-test-relay-count-is-limited ()
  "A forward refuses a connection beyond `rexel--relays-max'."
  (rexel-test--with-forwards sent buf
    (let* ((rexel--relays-max 2)
           (forward (rexel--forward-open "localhost" (rexel-test--free-port)))
           (clients (mapcar (lambda (_)
                              (rexel-test--client (rexel--forward-local-port forward)))
                            (list 1 2 3))))
      (rexel-test--until
       (lambda () (= 1 (seq-count (lambda (client) (process-get client :closed))
                                  clients))))
      (should (= 2 (length (rexel--forward-relays forward)))))))

(ert-deftest rexel-test-relay-stops-reading-a-connection-far-ahead ()
  "A connection is not read while its stream is far behind, and is again later."
  (rexel-test--with-forwards sent buf
    (let* ((conn rexel--conn)
           (rexel--relay-backlog-bytes 10000)
           (forward (rexel--forward-open "localhost" (rexel-test--free-port)))
           (client (rexel-test--client (rexel--forward-local-port forward)))
           (bytes (make-string 30000 ?x)))
      (rexel-test--tunnel conn sent)
      (rexel-test--until (lambda () (rexel-test--find "data.connect" sent)))
      ;; Before the stream is open nothing can be written.
      (process-send-string client bytes)
      (let ((relay (car (rexel--forward-relays forward))))
        (rexel-test--until
         (lambda () (eq t (process-filter (rexel--relay-client relay)))))
        (should (> (rexel--relay-backlog relay) 10000))
        (let ((ws (car (rexel-test--relay-welcome conn sent))))
          (rexel-test--until
           (lambda ()
             (equal bytes
                    (mapconcat #'cadr (cdr (rexel-test--sent-on ws)) ""))))
          (should (= 0 (rexel--relay-backlog relay)))
          (should (eq #'rexel--relay-filter
                      (process-filter (rexel--relay-client relay)))))))))

;;; Following the ports

(ert-deftest rexel-test-ports-update-names-or-forwards-new-ports ()
  (pcase-dolist (`(,option ,named ,forwarded) '((message t nil) (t nil t) (nil nil nil)))
    (rexel-test--with-forwards sent buf
      (let ((rexel-port-forward-new-ports option)
            (port (rexel-test--free-port))
            messages)
        (cl-letf (((symbol-function 'message)
                   (lambda (format &rest args) (push (apply #'format format args) messages))))
          ;; The ports a block has when it is attached to are not new.
          (rexel--ports-update (rexel-test--ports '(80)))
          (should-not messages)
          (should-not rexel--forwards)
          (rexel--ports-update (rexel-test--ports '(80) (list port nil "vite"))))
        (should (eq named (and (seq-some (lambda (m)
                                           (string-match-p
                                            (format "vite listens on port %d" port) m))
                                         messages)
                               t)))
        (should (eq forwarded (and rexel--forwards t)))
        (when forwarded
          (should (= port (rexel--forward-port (car rexel--forwards)))))))))

(ert-deftest rexel-test-ports-update-closes-forwards-of-closed-ports ()
  "A forward follows `rexel-port-forward-when-port-closes' once its port is gone."
  (pcase-dolist (`(,option ,closed ,again) '((remember t t) (close t nil) (keep nil nil)))
    (rexel-test--with-forwards sent buf
      (let* ((rexel-port-forward-when-port-closes option)
             (rexel-port-forward-new-ports nil)
             (port (rexel-test--free-port))
             (inhibit-message t)
             local)
        (rexel--ports-update (rexel-test--ports (list port)))
        (let ((forward (rexel--forward-open "localhost" port)))
          (setq local (rexel--forward-local-port forward))
          (rexel-test--tunnel rexel--conn sent)
          ;; A list that may leave out ports closes nothing.
          (rexel--ports-update '((status . "partial") (ports . [])))
          (should (memq forward rexel--forwards))
          (rexel--ports-update (rexel-test--ports))
          (should (eq closed (not (memq forward rexel--forwards))))
          (should (eq closed (and (rexel-test--find "block.close" sent) t))))
        (rexel--ports-update (rexel-test--ports (list port)))
        (if (not again)
            (should (eq (not closed) (and rexel--forwards t)))
          (should (= 1 (length rexel--forwards)))
          (should (= local (rexel--forward-local-port (car rexel--forwards))))
          (should-not rexel--forward-remembered))))))

(ert-deftest rexel-test-ports-update-leaves-other-forwards ()
  "A forward to a port the server never listed stays, as does one elsewhere."
  (rexel-test--with-forwards sent buf
    (let ((port (rexel-test--free-port))
          (inhibit-message t))
      (rexel--ports-update (rexel-test--ports))
      (rexel--forward-open "localhost" port)
      (rexel--forward-open "db.internal" 5432)
      (rexel--ports-update (rexel-test--ports '(5432)))
      (rexel--ports-update (rexel-test--ports))
      (should (= 2 (length rexel--forwards))))))

(ert-deftest rexel-test-ports-of-this-machines-server-are-not-followed ()
  (rexel-test--with-forwards sent buf
    (setf (rexel--conn-host rexel--conn) nil)
    (let ((rexel-port-forward-new-ports t))
      (rexel--ports-update (rexel-test--ports))
      (rexel--ports-update (rexel-test--ports '(5173)))
      (should-not rexel--forwards)
      (should-not sent)
      (should-error (rexel-port-forward 5173) :type 'user-error))))

(ert-deftest rexel-test-forward-setup-asks-for-ports-on-a-ready-connection ()
  "A new block buffer asks for its ports, unless its connection just dropped."
  (rexel-test--with-forwards sent buf
    (rexel--forward-setup)
    (should (equal "com.superlogical.terminal.ports" (alist-get 'method (car sent))))
    (should (eq rexel--forward-handler (car browse-url-handlers)))
    (setq sent nil)
    (setf (rexel--conn-client-id rexel--conn) nil)
    (cl-letf (((symbol-function 'rexel--send)
               (lambda (&rest _) (error "Sent on a closed connection"))))
      (rexel--forward-setup))
    (should (= 1 (seq-count (lambda (entry) (eq entry rexel--forward-handler))
                            browse-url-handlers)))))

(ert-deftest rexel-test-ports-changed-event-updates-the-block ()
  (rexel-test--with-forwards sent buf
    (let ((rexel-port-forward-new-ports nil))
      (rexel--on-event
       rexel--conn
       `((type . "session_event")
         (payload . ((session_id . "k")
                     (event . ((type . "block_event")
                               (payload . ((block_id . "block:00000k")
                                           (creator_name . "com.superlogical.terminal")
                                           (name . "ports_changed")
                                           (payload . ,(rexel-test--ports '(5173)))))))))))
      (should (equal '((5173 "localhost" "vite")) rexel--ports)))))


;;; Links and commands

(ert-deftest rexel-test-link-to-localhost-opens-through-a-forward ()
  (rexel-test--with-forwards sent buf
    (let* ((port (rexel-test--free-port))
           (inhibit-message t)
           opened-urls
           (browse-url-browser-function
            (lambda (url &rest _) (push url opened-urls))))
      (rexel--forward-setup)
      (setq sent nil)
      (browse-url (format "http://localhost:%d/app?q=1#top" port))
      (should (= 1 (length rexel--forwards)))
      (let ((local (rexel--forward-local-port (car rexel--forwards))))
        (should (equal (list (format "http://localhost:%d/app?q=1#top" local))
                       opened-urls))
        (should (equal "localhost" (rexel--forward-host (car rexel--forwards))))
        (should (= port (rexel--forward-port (car rexel--forwards))))
        ;; An address the listener is not on becomes 127.0.0.1.
        (browse-url (format "http://[::1]:%d/" port))
        (should (equal (format "http://127.0.0.1:%d/" local) (car opened-urls)))
        (should (= 1 (length rexel--forwards))))
      ;; Other links are left alone.
      (browse-url "https://example.com/")
      (should (equal "https://example.com/" (car opened-urls)))
      (should (= 1 (length rexel--forwards)))
      ;; So are all with the option off, and on this machine's server.
      (let ((rexel-port-forward-links nil))
        (browse-url "http://localhost:9/"))
      (setf (rexel--conn-host rexel--conn) nil)
      (browse-url "http://localhost:8/")
      (should (equal '("http://localhost:8/" "http://localhost:9/")
                     (seq-take opened-urls 2)))
      (should (= 1 (length rexel--forwards))))))

(ert-deftest rexel-test-port-forward-command ()
  "The command forwards a block's only port and reads one of several."
  (rexel-test--with-forwards sent buf
    (let ((a (rexel-test--free-port))
          (inhibit-message t)
          (kill-ring nil)
          asked)
      (cl-letf (((symbol-function 'rexel--prompt-later) #'funcall)
                ((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _)
                   (setq asked (mapcar #'car choices))
                   (seq-find (lambda (choice)
                               (not (string-suffix-p "(forwarded)" choice)))
                             asked)))
                ((symbol-function 'read-string)
                 (lambda (&rest _) (setq asked 'target) "db.internal:5432")))
        ;; One port: no question.
        (rexel-port-forward)
        (should (equal "com.superlogical.terminal.ports" (alist-get 'method (car sent))))
        (rexel-test--respond rexel--conn (car sent) (rexel-test--ports (list a)))
        (should-not asked)
        (should (= a (rexel--forward-port (car rexel--forwards))))
        (should (equal (list a) (mapcar #'car rexel--ports)))
        (should (equal (rexel--forward-url (car rexel--forwards)) (car kill-ring)))
        ;; Several: one is read, and the forwarded one says so.
        (let ((b (rexel-test--free-port)))
          (rexel-port-forward)
          (rexel-test--respond rexel--conn (rexel-test--find "com.superlogical.terminal.ports" sent)
                               (rexel-test--ports (list a) (list b nil "rails")))
          (should (equal (sort (list (format "%d vite (forwarded)" a)
                                     (format "%d rails" b))
                               #'string<)
                         (sort (copy-sequence asked) #'string<)))
          (should (= 2 (length rexel--forwards))))
        ;; None: a target is read.
        (rexel-port-forward)
        (rexel-test--respond rexel--conn (rexel-test--find "com.superlogical.terminal.ports" sent)
                             (rexel-test--ports))
        (should (eq asked 'target))
        (should (equal "db.internal" (rexel--forward-host (car rexel--forwards))))
        (should (= 5432 (rexel--forward-port (car rexel--forwards))))
        ;; From Lisp.
        (rexel-port-forward 8443 "api.internal")
        (should (equal "api.internal" (rexel--forward-host (car rexel--forwards))))))))

(ert-deftest rexel-test-port-forward-command-refusals ()
  "No port chosen and a session being reattached forward nothing."
  (rexel-test--with-forwards sent buf
    (let (messages)
      (cl-letf (((symbol-function 'rexel--prompt-later) #'funcall)
                ((symbol-function 'completing-read) (lambda (&rest _) ""))
                ((symbol-function 'message)
                 (lambda (format &rest args)
                   (push (apply #'format format args) messages))))
        (rexel-port-forward)
        (rexel-test--respond rexel--conn (car sent)
                             (rexel-test--ports '(80) '(81))))
      (should (equal '("No port chosen") messages))
      (should-not rexel--forwards)
      (setf (rexel--conn-attaching rexel--conn) (list "k"))
      (should-error (rexel-port-forward 80) :type 'user-error)
      (should-not rexel--forwards))))

(ert-deftest rexel-test-block-on-another-connection-forgets-its-ports ()
  "A buffer streamed again on another connection has no port known or forwarded."
  (rexel-test--with-forwards sent buf
    (let ((other (rexel--make-conn :endpoints (list "y") :host "studio"
                                   :client-id t :ws (rexel--conn-ws rexel--conn))))
      (rexel--forward-open "localhost" (rexel-test--free-port))
      (setq rexel--ports '((5173 "localhost" "vite"))
            rexel--forward-remembered '((5173 . 5173)))
      (cl-letf (((symbol-function 'ghostel-make-terminal)
                 (lambda (buffer &rest _) buffer))
                ((symbol-function 'rexel-block-mode) #'ignore)
                ((symbol-function 'rexel--open-data-connection) #'ignore))
        (rexel--attach-block-buffer rexel--conn "k" "block:00000k" buf)
        (should (equal '((5173 . 5173)) rexel--forward-remembered))
        (should (= 1 (length rexel--forwards)))
        (rexel--attach-block-buffer other "k" "block:00000k" buf)
        (should (eq 'unknown rexel--ports))
        (should-not rexel--forward-remembered)
        (should-not rexel--forwards)))))

(ert-deftest rexel-test-port-forward-close-command ()
  (rexel-test--with-forwards sent buf
    (let ((inhibit-message t))
      (should-error (call-interactively #'rexel-port-forward-close) :type 'user-error)
      (rexel--forward-open "localhost" (rexel-test--free-port))
      (call-interactively #'rexel-port-forward-close)
      (should-not rexel--forwards))))

(provide 'rexel-port-forward-test)
;;; rexel-port-forward-test.el ends here
