;;; rexel-test.el --- Tests for rexel -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests of what needs no server: the stream parsing, the handling of
;; server addresses, and the connection logic with WebSockets stubbed
;; and timers run by hand.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'rexel)

(defun rexel-test--record (tag payload)
  "Return a snapshot record with TAG and PAYLOAD, CRC left zero."
  (concat (unibyte-string (logand tag #xff) (ash tag -8)
                          (logand (length payload) #xff)
                          (logand (ash (length payload) -8) #xff)
                          (logand (ash (length payload) -16) #xff)
                          (logand (ash (length payload) -24) #xff)
                          0 0 0 0)
          payload))

(defconst rexel-test--snapshot
  (concat "GHOSTSNP\1\0"
          (rexel-test--record 1 (make-string 300 ?x))
          (rexel-test--record 5 "")
          (rexel-test--record 6 ""))
  "A fake snapshot: envelope, one 300-byte record, READY, FINISH.")

(ert-deftest rexel-test-snapshot-walk ()
  (let ((len (length rexel-test--snapshot)))
    (should (equal (list t len) (rexel--snapshot-walk rexel-test--snapshot 10)))
    (should (equal (list t len) (rexel--snapshot-walk
                                 (concat rexel-test--snapshot "live bytes") 10)))
    ;; Stops at the start of the incomplete 300-byte record.
    (should (equal '(nil 10 310) (rexel--snapshot-walk
                                  (substring rexel-test--snapshot 0 100) 10)))
    ;; Its header is cut off.
    (should (equal '(nil 10 10) (rexel--snapshot-walk
                                 (substring rexel-test--snapshot 0 15) 10)))
    ;; FINISH header seen but a byte short of complete.
    (should (equal (list nil (- len 10) 10)
                   (rexel--snapshot-walk (substring rexel-test--snapshot 0 -1) 10)))
    (should-error (rexel--snapshot-walk
                   (concat (make-string 10 ?e) (rexel-test--record 1 "")
                           (unibyte-string 1 0 0 0 0 #x20 0 0 0 0))
                   20))))

(ert-deftest rexel-test-snapshot-in-chunks ()
  (with-temp-buffer
    (let ((stream (concat rexel-test--snapshot "live"))
          restored live)
      (setq rexel--data-state 'snapshot rexel--stream (rexel--make-stream))
      (cl-letf (((symbol-function 'ghostel--restore-snapshot)
                 (lambda (_term s) (setq restored s)))
                ((symbol-function 'ghostel--apply-palette) #'ignore)
                ((symbol-function 'ghostel--apply-bold-config) #'ignore)
                ((symbol-function 'ghostel--set-size-with-cell-dims) #'ignore)
                ((symbol-function 'ghostel--invalidate) #'ignore)
                ((symbol-function 'rexel--sync-size) #'ignore)
                ((symbol-function 'rexel--on-live-bytes)
                 (lambda (b) (setq live b))))
        (dolist (range '((0 . 5) (5 . 50) (50 . 320) (320 . 335) (335 . nil)))
          (rexel--on-snapshot-bytes (substring stream (car range) (cdr range))))
        (should (equal rexel-test--snapshot restored))
        (should (equal "live" live))
        (should (eq 'live rexel--data-state))))))

(ert-deftest rexel-test-snapshot-rejects-other-streams ()
  (with-temp-buffer
    (setq rexel--data-state 'snapshot rexel--stream (rexel--make-stream))
    (rexel--on-snapshot-bytes "not a snapshot at all")
    (should (eq 'closed rexel--data-state))
    (should-not rexel--stream)))

(ert-deftest rexel-test-snapshot-ends-on-oversized-record ()
  (with-temp-buffer
    (setq rexel--data-state 'snapshot rexel--stream (rexel--make-stream))
    (rexel--on-snapshot-bytes (substring rexel-test--snapshot 0 100))
    (should (rexel--stream-snapshot rexel--stream))
    (rexel--on-snapshot-bytes
     (concat (substring rexel-test--snapshot 100 320)
             (unibyte-string 1 0 0 0 0 #x20 0 0 0 0)))
    (should (eq 'closed rexel--data-state))
    (should-not rexel--stream)))

(ert-deftest rexel-test-snapshot-restore-failure-ends-stream ()
  (with-temp-buffer
    (setq rexel--data-state 'snapshot rexel--stream (rexel--make-stream))
    (cl-letf (((symbol-function 'ghostel--restore-snapshot)
               (lambda (&rest _) (error "Bad CRC"))))
      (rexel--on-snapshot-bytes rexel-test--snapshot))
    (should (eq 'closed rexel--data-state))))

(defconst rexel-test--marker "\e_rex1;42;")

(ert-deftest rexel-test-split-stream-plain ()
  (should (equal '(("hello") . "")
                 (rexel--split-stream "hello" rexel-test--marker))))

(ert-deftest rexel-test-split-stream-message ()
  (should (equal '(("a" (message . "{\"type\":\"resize\"}") "b") . "")
                 (rexel--split-stream
                  (concat "a" rexel-test--marker "{\"type\":\"resize\"}\e\\b")
                  rexel-test--marker))))

(ert-deftest rexel-test-split-stream-wrong-nonce-passes-through ()
  (should (equal '(("\e_rex1;7;{}\e\\") . "")
                 (rexel--split-stream "\e_rex1;7;{}\e\\" rexel-test--marker))))

(ert-deftest rexel-test-split-stream-holds-partial-marker ()
  (should (equal '(("out") . "\e_re")
                 (rexel--split-stream "out\e_re" rexel-test--marker)))
  (should (equal '(("out") . "\e")
                 (rexel--split-stream "out\e" rexel-test--marker)))
  ;; A kitty APC is not a prefix of the marker beyond ESC _.
  (should (equal '(("\e_Gf=100") . "")
                 (rexel--split-stream "\e_Gf=100" rexel-test--marker))))

(ert-deftest rexel-test-split-stream-holds-unterminated-message ()
  (pcase-let ((`(,items . ,pending)
               (rexel--split-stream (concat "x" rexel-test--marker "{\"ty")
                                    rexel-test--marker)))
    (should (equal '("x") items))
    (should (equal (concat rexel-test--marker "{\"ty") pending))
    ;; Completing it on the next chunk yields the message.
    (should (equal '((message . "{\"type\":\"theme\"}") "y")
                   (car (rexel--split-stream (concat pending "pe\":\"theme\"}\e\\y")
                                             rexel-test--marker))))))

(ert-deftest rexel-test-split-stream-passes-overlong-message ()
  (let ((data (concat rexel-test--marker (make-string rexel--max-message-bytes ?j))))
    (should (equal (cons (list data) "")
                   (rexel--split-stream data rexel-test--marker)))
    ;; At the limit it is still held back.
    (setq data (substring data 0 rexel--max-message-bytes))
    (should (equal (cons nil data)
                   (rexel--split-stream data rexel-test--marker)))))

(ert-deftest rexel-test-old-stream-is-ignored ()
  (with-temp-buffer
    (let ((stream (rexel--make-stream)))
      (setq rexel--data-state 'snapshot rexel--stream stream)
      (rexel--on-data (current-buffer) (rexel--make-stream) "not a snapshot at all")
      (rexel--on-data-close (current-buffer) (rexel--make-stream))
      (should (eq 'snapshot rexel--data-state))
      (should (eq stream rexel--stream)))))

(ert-deftest rexel-test-u64le ()
  (should (= 1 (rexel--u64le (unibyte-string 1 0 0 0 0 0 0 0) 0)))
  (should (= (ash 1 56) (rexel--u64le (unibyte-string 9 0 0 0 0 0 0 0 1) 1))))

(ert-deftest rexel-test-welcome-reads-nonce ()
  (with-temp-buffer
    (setq rexel--data-state 'welcome rexel--stream (rexel--make-stream))
    (rexel--on-welcome (concat (unibyte-string #x81) (make-string 16 ?t)
                               (unibyte-string 0 0 0 1 0 0 0)
                               (unibyte-string 2 1 0 0 0 0 0 0)))
    (should (eq rexel--data-state 'snapshot))
    (should (equal "\e_rex1;258;" (rexel--stream-marker rexel--stream)))))

(defun rexel-test--welcome (token)
  "Return a WELCOME carrying TOKEN and channel nonce 258."
  (concat (unibyte-string #x81) token (unibyte-string 0 0 0 1 0 0 0)
          (unibyte-string 2 1 0 0 0 0 0 0)))

(ert-deftest rexel-test-dropped-stream-resumes ()
  (rexel-test--with-fake-conn _sent
    (let ((ws (list 'fake)) hello)
      (cl-letf (((symbol-function 'rexel--ws-open)
                 (lambda (_endpoint _path _protocol on-open &rest _)
                   (funcall on-open ws) ws))
                ((symbol-function 'websocket-send)
                 (lambda (_ws frame) (setq hello (websocket-frame-payload frame))))
                ((symbol-function 'rexel--on-live-bytes) #'ignore))
        (setq rexel--data-state 'welcome rexel--stream (rexel--make-stream))
        (let ((s rexel--stream))
          (rexel--on-welcome (rexel-test--welcome (make-string 16 ?t)))
          (setq rexel--data-state 'live)
          (rexel--on-data (current-buffer) s (make-string 300 ?x))
          (rexel--on-data-close (current-buffer) s)
          (should (eq rexel--data-state 'welcome))
          (should (equal hello (concat (unibyte-string 2) (make-string 16 ?t)
                                       (unibyte-string 44 1 0 0 0 0 0 0))))
          (rexel--on-data (current-buffer) s (rexel-test--welcome (make-string 16 ?u)))
          (should (eq rexel--data-state 'live))
          (should (equal (make-string 16 ?u) (rexel--stream-token s)))
          (should (= 300 (rexel--stream-received s)))
          ;; A connection that drops before its WELCOME ends the stream.
          (rexel--on-data-close (current-buffer) s)
          (rexel--on-data-close (current-buffer) s)
          (should (eq rexel--data-state 'closed)))))))

(ert-deftest rexel-test-welcome-reject ()
  "Only a refused RESUME has the block streamed afresh."
  (rexel-test--with-fake-conn _sent
    (let ((conn rexel--conn)
          (buf (current-buffer))
          refreshed)
      (cl-letf (((symbol-function 'rexel--schedule-view-refresh)
                 (lambda (_conn sid) (push sid refreshed)))
                ((symbol-function 'websocket-send) #'ignore))
        (dolist (resume '(nil live))
          (let* ((ws (rexel-test--ws))
                 (stream (rexel--make-stream :ws ws :resume resume)))
            (setf (websocket-on-close ws)
                  (lambda (_ws) (rexel--on-data-close buf stream)))
            (puthash "b" buf (rexel--conn-blocks conn))
            (setq rexel--data-state 'welcome rexel--stream stream)
            (rexel--on-welcome (unibyte-string #x82 3))
            (should (eq rexel--data-state 'closed))
            (should-not rexel--stream)
            (should-not (process-live-p (websocket-conn ws)))
            (should (equal (and resume '("s")) refreshed))
            (should (eq (null resume)
                        (eq buf (gethash "b" (rexel--conn-blocks conn)))))))))))

(ert-deftest rexel-test-control-welcome-checks-revision ()
  (pcase-dolist (`(,protocol ,revision ,ok)
                 '(("rex.control.v1" 8 t) ("rex.control.v1" 7 nil)
                   ("rex.control.v1" nil nil) ("other" 8 nil)))
    (let ((conn (rexel--make-conn :endpoints (list "none")))
          (ready nil)
          (closed nil))
      (push (lambda (_conn) (setq ready t)) (rexel--conn-on-ready conn))
      (cl-letf (((symbol-function 'websocket-close)
                 (lambda (_ws) (setq closed t))))
        (rexel--on-control-message
         conn (json-serialize
               `((type . "welcome") (protocol . ,protocol)
                 ,@(when revision `((api_revision . ,revision)))))))
      (should (eq ready ok))
      (should (eq closed (not ok)))
      (should (eq (and (rexel--conn-client-id conn) t) ok)))))

(ert-deftest rexel-test-hosts-merge ()
  (let* ((rexel-hosts '(("studio" . "/tmp/studio.sock")
                        ("attic" "attic.ts.net" "http://10.0.0.2:7297")
                        ("bad" . "ws://bad.ts.net")
                        (nil . "/tmp/unlabelled.sock")
                        ("broken")))
         (local (make-temp-file "rexel"))
         (endpoint (concat "unix://" (url-hexify-string local))))
    (unwind-protect
        (cl-letf (((symbol-function 'rexel--socket-live-p) #'file-exists-p)
                  ((symbol-function 'rexel--rex-json)
                   (lambda (&rest args)
                     (if (equal (car args) "server")
                         `((endpoint . ,endpoint))
                       '((hosts . [((label . "Studio") (endpoints . ["studio.ts.net"]))
                                   ((label . "far") (endpoints . ["far.ts.net"]))
                                   ((label . "none") (endpoints . ["ws://none.ts.net"]))
                                   ((label . "near")
                                    (endpoints . ["unix:///tmp/near.sock" "near.ts.net"]))]))))))
          (should (equal `((nil ,local)
                           ("studio" "/tmp/studio.sock")
                           ("attic" "https://attic.ts.net" "http://10.0.0.2:7297")
                           ("far" "https://far.ts.net")
                           ("near" "/tmp/near.sock" "https://near.ts.net"))
                         (rexel--hosts))))
      (delete-file local))))

(ert-deftest rexel-test-normalize-endpoint ()
  (pcase-dolist (`(,address ,endpoint)
                 '(("unix:///tmp/a%20b.sock" "/tmp/a b.sock")
                   ("UNIX:///tmp/rex.sock" "/tmp/rex.sock")
                   ("/tmp/rex.sock" "/tmp/rex.sock")
                   ("studio.ts.net" "https://studio.ts.net")
                   ("studio.ts.net/" "https://studio.ts.net")
                   ("10.0.0.2:7297" "https://10.0.0.2:7297")
                   ("[fd7a::1]:7297" "https://[fd7a::1]:7297")
                   ("http://10.0.0.2:7297//" "http://10.0.0.2:7297")
                   ("HTTPS://Studio.ts.net" "https://Studio.ts.net")
                   ("https://studio.ts.net/rex/" "https://studio.ts.net/rex")
                   ("ws://studio.ts.net" nil)
                   ("https://" nil)
                   ("unix://" nil)
                   ("unix://rel.sock" nil)
                   ("rel.sock/" "https://rel.sock")
                   ("https://studio.ts.net?x=1" nil)
                   (" studio.ts.net" nil)
                   ("https://me@studio.ts.net" nil)
                   ("https://me:secret@studio.ts.net" nil)
                   ("me:secret@studio.ts.net" nil)
                   ("" nil)
                   (nil nil)))
    (should (equal endpoint (rexel--normalize-endpoint address))))
  (should (equal (expand-file-name "~/rex.sock") (rexel--normalize-endpoint "~/rex.sock")))
  (let ((case-fold-search nil))
    (should (equal "https://h" (rexel--normalize-endpoint "HTTPS://h")))
    (should (rexel--network-endpoint-p "https://h")))
  (let ((case-fold-search t))
    (should-not (rexel--network-endpoint-p "HTTPS://h"))))

(defmacro rexel-test--with-dials (dialled &rest body)
  "Run BODY with `rexel--ws-open' stubbed to collect in DIALLED.
Each dial pushes (ENDPOINT WS ON-OPEN ON-CLOSE); the endpoint \"dead\"
signals.  `rexel--on-control-close' sets the variable `closed'."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'rexel--ws-open)
              (lambda (endpoint _path _protocol on-open _on-message on-close)
                (when (equal endpoint "dead") (error "Refused"))
                (let ((ws (list endpoint)))
                  (push (list endpoint ws on-open on-close) ,dialled)
                  ws)))
             ((symbol-function 'rexel--send-hello) #'ignore)
             ((symbol-function 'rexel--on-control-close)
              (lambda (_conn) (setq closed t))))
     ,@body))

(defun rexel-test--dial-event (dial event)
  "Run the on-open or on-close callback of DIAL, as EVENT says."
  (pcase-let ((`(,_ ,ws ,on-open ,on-close) dial))
    (funcall (if (eq event 'open) on-open on-close) ws)))

(ert-deftest rexel-test-dial-tries-endpoints-in-order ()
  (let ((conn (rexel--make-conn :endpoints (list "dead" "a" "b" "c")))
        dialled closed)
    (rexel-test--with-dials dialled
      (rexel--dial conn)
      (should (equal '("a") (mapcar #'car dialled)))
      ;; The endpoint in use is the one that answered last.
      (should-not (rexel--conn-endpoint conn))
      ;; Closing without a welcome moves on to the next endpoint.
      (rexel-test--dial-event (car dialled) 'close)
      (should (equal '("b" "a") (mapcar #'car dialled)))
      (rexel-test--dial-event (car dialled) 'open)
      (should (equal "b" (rexel--conn-endpoint conn)))
      (should-not closed)
      ;; A close of the replaced connection is ignored.
      (rexel-test--dial-event (cadr dialled) 'close)
      (should (= 2 (length dialled)))
      (should-not closed)
      ;; After a welcome a close ends the round, with \"c\" left untried.
      (cl-letf (((symbol-function 'rexel--reattach) #'ignore))
        (rexel--on-control-message
         conn "{\"type\":\"welcome\",\"protocol\":\"rex.control.v1\",\"api_revision\":8}"))
      (rexel-test--dial-event (car dialled) 'close)
      (should closed)
      (should (= 2 (length dialled))))))

(ert-deftest rexel-test-dial-out-of-endpoints ()
  (let ((conn (rexel--make-conn :endpoints (list "a" "dead")))
        dialled closed)
    (rexel-test--with-dials dialled
      (should-error (rexel--dial (rexel--make-conn :endpoints (list "dead"))))
      (rexel--dial conn)
      (rexel-test--dial-event (car dialled) 'close)
      (should closed))))

(ert-deftest rexel-test-refused-welcome-ends-the-round ()
  "A server of another API revision is not tried at its other endpoints."
  (let ((conn (rexel--make-conn :endpoints (list "a" "b")))
        dialled closed)
    (rexel-test--with-dials dialled
      (cl-letf (((symbol-function 'websocket-close)
                 (lambda (_ws) (rexel-test--dial-event (car dialled) 'close))))
        (rexel--dial conn)
        (rexel--on-control-message
         conn "{\"type\":\"welcome\",\"protocol\":\"rex.control.v1\",\"api_revision\":7}")
        (should closed)
        (should (= 1 (length dialled)))))))

(ert-deftest rexel-test-control-close-drops-waiting-callbacks ()
  "A connection that closes unwelcomed reports to those waiting for it."
  (let* ((conn (rexel--make-conn :endpoints (list "none")))
         (rexel--conns (list conn)))
    (cl-letf (((symbol-function 'rexel--dial) #'ignore))
      (setf (alist-get "s" (rexel--conn-sessions conn) nil nil #'equal) "work")
      (push #'ignore (rexel--conn-on-ready conn))
      (rexel--on-control-close conn)
      (should-not (rexel--conn-on-ready conn))
      (should (rexel--conn-timer conn))
      (cancel-timer (rexel--conn-timer conn)))))

(ert-deftest rexel-test-ws-url ()
  (should (equal "ws://rex/api/control" (rexel--ws-url "/tmp/rex.sock" "/api/control")))
  (should (equal "wss://studio.ts.net/api/data"
                 (rexel--ws-url "https://studio.ts.net" "/api/data")))
  (should (equal "ws://10.0.0.2:7297/api/data"
                 (rexel--ws-url "http://10.0.0.2:7297" "/api/data"))))

(ert-deftest rexel-test-buffer-name-carries-host ()
  (let ((conn (rexel--make-conn :endpoints (list "none") :host "studio")))
    (setf (alist-get "s" (rexel--conn-sessions conn) nil nil #'equal) "work")
    (should (equal "*rex:work@studio:123456*"
                   (rexel--buffer-name conn "s" "block:123456")))))

(ert-deftest rexel-test-apply-view-renames-on-label-change ()
  (let* ((conn (rexel--make-conn :endpoints (list "none")))
         (buf (generate-new-buffer "rexel-test"))
         (view '((label . "work")
                 (windows . [((layers . [((blocks . [((block_id . "block:123456")
                                                      (creator_name . "com.superlogical.terminal"))]))]))]))))
    (with-current-buffer buf
      (setq rexel--session-id "label-test" rexel--block-id "block:123456"))
    (puthash "block:123456" buf (rexel--conn-blocks conn))
    (unwind-protect
        (progn
          (rexel--apply-view conn "label-test" view)
          (should (equal "*rex:work:123456*" (buffer-name buf)))
          (rexel--apply-view conn "label-test" `((label . "play") ,@(cdr view)))
          (should (equal "*rex:play:123456*" (buffer-name buf))))
      (kill-buffer buf))))

(defmacro rexel-test--with-fake-conn (sent &rest body)
  "Run BODY with a fake connection whose sent objects collect in SENT."
  (declare (indent 1))
  `(let ((rexel--conn (rexel--make-conn
                       :endpoints (list "none") :client-id t
                       :ws (websocket-inner-create :conn nil :url "u"
                                                   :ready-state 'closed)))
         (,sent nil))
     (cl-letf (((symbol-function 'rexel--send)
                (lambda (_conn object) (push object ,sent))))
       (with-temp-buffer
         (setq rexel--session-id "s" rexel--block-id "b")
         ,@body))))

(defun rexel-test--sent-data (object)
  "Return the typed text in the write request OBJECT."
  (alist-get 'data (alist-get 'args (alist-get 'payload object))))

(ert-deftest rexel-test-hello-carries-reconnect-token ()
  (rexel-test--with-fake-conn sent
    (setf (rexel--conn-token rexel--conn) "tok")
    (cl-letf (((symbol-function 'rexel--ws-open)
               (lambda (_endpoint _path _protocol on-open &rest _)
                 (funcall on-open 'ws) 'ws)))
      (rexel--dial rexel--conn))
    (should (equal "tok" (alist-get 'reconnect_token (car sent))))))

(ert-deftest rexel-test-control-close-redials-while-attached ()
  (let* ((conn (rexel--make-conn :endpoints (list "none")))
         (rexel--conns (list conn))
         (dialled nil))
    (cl-letf (((symbol-function 'rexel--dial) (lambda (c) (push c dialled))))
      (rexel--on-control-close conn)
      (should-not rexel--conns)
      (setq rexel--conns (list conn))
      (setf (alist-get "s" (rexel--conn-sessions conn) nil nil #'equal) "work")
      (rexel--on-control-close conn)
      (should (memq conn rexel--conns))
      (should (= 2 (rexel--conn-delay conn)))
      (timer-event-handler (rexel--conn-timer conn))
      (should (equal (list conn) dialled))
      (should-not (rexel--conn-timer conn)))))

(ert-deftest rexel-test-welcome-after-drop-reattaches ()
  (rexel-test--with-fake-conn sent
    (let ((conn rexel--conn))
      (setf (alist-get "s" (rexel--conn-sessions conn) nil nil #'equal) "work"
            (rexel--conn-delay conn) 4)
      (rexel--on-control-message
       conn (json-serialize
             '((type . "welcome") (protocol . "rex.control.v1") (api_revision . 8)
               (client . ((id . "client:1") (reconnect_token . "tok") (resumed . t))))))
      (should (equal "tok" (rexel--conn-token conn)))
      (should-not (rexel--conn-delay conn))
      (should (equal "session.attach" (alist-get 'method (car sent))))
      (should (equal "s" (alist-get 'session_id (alist-get 'payload (car sent))))))))

(ert-deftest rexel-test-pty-out-serializes-and-coalesces ()
  (rexel-test--with-fake-conn sent
    (rexel--pty-out "a")
    (rexel--pty-out "b")
    (rexel--pty-out "c")
    ;; Only one write in flight; later bytes wait.
    (should (= 1 (length sent)))
    (should (equal "a" (rexel-test--sent-data (car sent))))
    ;; Its response releases the coalesced rest.
    (funcall (gethash (alist-get 'id (car sent)) (rexel--conn-pending rexel--conn))
             nil nil)
    (should (= 2 (length sent)))
    (should (equal "bc" (rexel-test--sent-data (car sent))))))

(ert-deftest rexel-test-pty-out-send-error-does-not-wedge ()
  (rexel-test--with-fake-conn sent
    (cl-letf (((symbol-function 'rexel--send)
               (lambda (&rest _) (error "Boom"))))
      (rexel--pty-out "\377"))
    (should-not rexel--out-inflight)
    (rexel--pty-out "x")
    (should (equal "x" (rexel-test--sent-data (car sent))))))

(defun rexel-test--respond (conn object)
  "Answer the request OBJECT on CONN with an empty result."
  (funcall (gethash (alist-get 'id object) (rexel--conn-pending conn)) nil nil))

(ert-deftest rexel-test-size-requests-serialize-and-take ()
  (rexel-test--with-fake-conn sent
    (setq rexel--data-state 'live)
    (rexel--resize 24 80 t)
    (should (equal '("session.own_block")
                   (mapcar (lambda (o) (alist-get 'method o)) sent)))
    ;; Sizes requested meanwhile wait; the latest one follows the take.
    (rexel--resize 30 100)
    (rexel--resize 40 120)
    (should (= 1 (length sent)))
    (rexel-test--respond rexel--conn (car sent))
    (should (equal '((columns . 120) (rows . 40))
                   (alist-get 'args (alist-get 'payload (car sent)))))
    ;; An unchanged size sends nothing more.
    (rexel-test--respond rexel--conn (car sent))
    (rexel--resize 40 120)
    (should (= 2 (length sent)))
    ;; A resize without take sends no own_block.
    (rexel--resize 20 90)
    (should (equal "com.superlogical.terminal.resize"
                   (alist-get 'method (car sent))))
    (should (= 3 (length sent)))))

(ert-deftest rexel-test-size-requests-wait-for-live ()
  (rexel-test--with-fake-conn sent
    (setq rexel--data-state 'snapshot)
    (rexel--resize 24 80 t)
    (should-not sent)
    (should-not rexel--requested-size)))

(ert-deftest rexel-test-sync-size-takes-with-fitted-size ()
  (rexel-test--with-fake-conn sent
    (setq rexel--data-state 'live)
    (cl-letf (((symbol-function 'ghostel--desired-size) (lambda () nil)))
      (rexel--sync-size))
    (should-not sent)
    (cl-letf (((symbol-function 'ghostel--desired-size) (lambda () '(80 . 24))))
      (rexel--sync-size))
    (should (equal "session.own_block" (alist-get 'method (car sent))))
    (rexel-test--respond rexel--conn (car sent))
    (should (equal '((columns . 80) (rows . 24))
                   (alist-get 'args (alist-get 'payload (car sent)))))))

(ert-deftest rexel-test-ws-filter-buffers-whole-frames ()
  (let* ((ws (websocket-inner-create :conn nil :url "u" :ready-state 'open))
         (proc (make-pipe-process :name "rexel-test" :noquery t))
         ;; Binary frame with a 16-bit length of 300.
         (frame (concat (unibyte-string #x82 126 1 44) (make-string 300 ?x)))
         calls)
    (unwind-protect
        (cl-letf (((symbol-function 'websocket-outer-filter)
                   (lambda (ws out)
                     (push out calls)
                     ;; Like websocket.el: keep input until the frame is whole.
                     (let ((text (concat (websocket-inflight-input ws) out)))
                       (setf (websocket-inflight-input ws)
                             (and (< (length text) (length frame)) text))))))
          (process-put proc :websocket ws)
          (dolist (range '((0 . 10) (10 . 100) (100 . 303) (303 . nil)))
            (rexel--ws-filter proc (substring frame (car range) (cdr range))))
          (should (equal (list (substring frame 10) (substring frame 0 10))
                         calls))
          (should-not (websocket-inflight-input ws))
          (should (<= (- (float-time) 5) (process-get proc :last-read))))
      (delete-process proc))))

(ert-deftest rexel-test-ws-filter-drops-ping-behind-closing-message ()
  (let ((ws (rexel-test--ws))
        (messages 0))
    (setf (websocket-on-message ws)
          (lambda (ws _frame)
            (cl-incf messages)
            (delete-process (websocket-conn ws))))
    (unwind-protect
        (progn
          ;; A binary frame, then a ping.
          (rexel--ws-filter (websocket-conn ws)
                            (unibyte-string #x82 1 ?m #x89 1 ?p))
          (should (= 1 messages)))
      (rexel-test--ws-delete ws))))

(defun rexel-test--ws (&optional state silence)
  "Return a WebSocket in STATE, by default `open', on a pipe process.
The process was last read from SILENCE seconds ago, by default now."
  (let* ((process (make-pipe-process :name "rexel-test" :noquery t))
         (ws (websocket-inner-create :conn process :url "u"
                                     :ready-state (or state 'open))))
    (process-put process :websocket ws)
    (process-put process :last-read (- (float-time) (or silence 0)))
    ws))

(defun rexel-test--ws-delete (ws)
  "Delete the process of WS and its pending probe."
  (let ((process (websocket-conn ws)))
    (when (process-get process :probe)
      (cancel-timer (process-get process :probe)))
    (delete-process process)))

(ert-deftest rexel-test-keepalive-probes-silent-connections ()
  (let* ((silent (rexel-test--ws 'open 100))
         (recent (rexel-test--ws 'open))
         (opening (rexel-test--ws 'connecting 100))
         (all (list silent recent opening))
         (rexel--keepalive-timer nil)
         pinged)
    (unwind-protect
        (cl-letf (((symbol-function 'websocket-send)
                   (lambda (ws frame)
                     (push (list ws (websocket-frame-opcode frame)
                                 (websocket-frame-payload frame))
                           pinged))))
          (rexel--keepalive)
          (should (= 1 (length pinged)))
          (should (eq silent (caar pinged)))
          ;; The server closes on a ping without payload.
          (should (equal '(ping "rexel") (cdar pinged)))
          (should (process-get (websocket-conn silent) :probe))
          (should-not (process-get (websocket-conn opening) :probe))
          (should (memq rexel--keepalive-timer timer-list))
          ;; With no connection left the timer stops.
          (mapc #'rexel-test--ws-delete all)
          (rexel--keepalive)
          (should-not rexel--keepalive-timer))
      (mapc #'rexel-test--ws-delete all)
      (when rexel--keepalive-timer
        (cancel-timer rexel--keepalive-timer)))))

(ert-deftest rexel-test-probe-closes-unanswered-connection ()
  (let* ((ws (rexel-test--ws 'open 1))
         (process (websocket-conn ws))
         (pings 0))
    (unwind-protect
        (cl-letf (((symbol-function 'websocket-send)
                   (lambda (&rest _) (cl-incf pings))))
          (rexel--probe ws)
          ;; One probe at a time.
          (rexel--probe ws)
          (should (= 1 pings))
          ;; Something arrived meanwhile.
          (process-put process :last-read (+ (float-time) 1))
          (timer-event-handler (process-get process :probe))
          (should (process-live-p process))
          (should-not (process-get process :probe))
          (process-put process :last-read (- (float-time) 1))
          (rexel--probe ws)
          (should (= 2 pings))
          ;; An answer Emacs has not read yet counts.
          (cl-letf (((symbol-function 'accept-process-output)
                     (lambda (process &rest _)
                       (process-put process :last-read (+ (float-time) 1)))))
            (timer-event-handler (process-get process :probe)))
          (should (process-live-p process))
          (process-put process :last-read (- (float-time) 1))
          (rexel--probe ws)
          (should (= 3 pings))
          (timer-event-handler (process-get process :probe))
          (should-not (process-live-p process)))
      (rexel-test--ws-delete ws))))

(ert-deftest rexel-test-probe-send-error-closes-connection ()
  (let ((ws (rexel-test--ws)))
    (unwind-protect
        (cl-letf (((symbol-function 'websocket-send)
                   (lambda (&rest _) (error "Gone"))))
          (rexel--probe ws)
          (should-not (process-live-p (websocket-conn ws))))
      (rexel-test--ws-delete ws))))

(ert-deftest rexel-test-pty-out-probes-unanswered-write ()
  (rexel-test--with-fake-conn _sent
    (let ((control (rexel-test--ws))
          (data (rexel-test--ws))
          pinged)
      (setf (rexel--conn-ws rexel--conn) control)
      (setq rexel--stream (rexel--make-stream :ws data))
      (unwind-protect
          (cl-letf (((symbol-function 'websocket-send)
                     (lambda (ws _frame) (push ws pinged))))
            (rexel--pty-out "a")
            (should-not pinged)
            (rexel--pty-out "b")
            (rexel--pty-out "c")
            ;; The control and the data connection, once each.
            (should (= 2 (length pinged)))
            (should (memq control pinged))
            (should (memq data pinged)))
        (rexel-test--ws-delete control)
        (rexel-test--ws-delete data)))))

(ert-deftest rexel-test-shell-process ()
  "The shell is the process under the login wrapper, else the foreground."
  (let ((fg '((name . "sleep") (pid . 3)))
        (bash '((name . "bash") (pid . 2)))
        (login '((name . "login") (pid . 1))))
    (should-not (rexel--shell-process nil nil))
    (should (eq (rexel--shell-process fg nil) fg))
    (should (eq (rexel--shell-process fg (vector login)) fg))
    (should (eq (rexel--shell-process fg (vector bash login)) bash))
    (should (eq (rexel--shell-process fg (vector bash)) bash))
    (should (eq (rexel--shell-process fg (vector '((name . "sudo")) bash login))
                bash))))

(ert-deftest rexel-test-replies-dropped ()
  "Replies made while parsing the stream are dropped, typed bytes are sent."
  (with-temp-buffer
    (let ((reply (lambda (&rest _) (ghostel--pty-out "\e[?62;22c") 0)))
      (setq rexel--conn 'fake
            rexel--stream (rexel--make-stream :marker "\e_rex1;1;")
            ghostel--term 'fake
            ghostel-pty-out-function #'rexel--pty-out)
      (cl-letf (((symbol-function 'ghostel--write-vt) reply)
                ((symbol-function 'ghostel--restore-snapshot) reply)
                ((symbol-function 'ghostel--apply-palette) reply)
                ((symbol-function 'ghostel--apply-bold-config) #'ignore)
                ((symbol-function 'ghostel--set-size-with-cell-dims) #'ignore)
                ((symbol-function 'ghostel--invalidate) #'ignore)
                ((symbol-function 'rexel--sync-size) #'ignore)
                ((symbol-function 'rexel--flush-out) #'ignore))
        (rexel--on-live-bytes "\e[c")
        (should-not rexel--out-pending)
        (setf (rexel--stream-snapshot rexel--stream) (list "snapshot"))
        (rexel--restore-snapshot)
        (should (eq 'live rexel--data-state))
        (should-not rexel--out-pending)
        (ghostel--pty-out "typed")
        (should (equal "typed" rexel--out-pending))))))

(ert-deftest rexel-test-read-host-starts-server ()
  "The server starts only with no host known and as the option allows."
  (let* ((local '(nil "/tmp/rex.sock"))
         (rexel-program "rex")
         hosts started asked answer
         (read-host
          (lambda (option known)
            (setq hosts known started nil asked nil)
            (let ((rexel-autostart-server option))
              (ignore-errors (rexel--read-host))))))
    (cl-letf (((symbol-function 'rexel--hosts) (lambda () hosts))
              ((symbol-function 'executable-find) (lambda (_) "/bin/rex"))
              ((symbol-function 'y-or-n-p) (lambda (_) (setq asked t) answer))
              ((symbol-function 'call-process)
               (lambda (_ _ _ _ &rest args)
                 (setq started args hosts (list local))
                 0)))
      (should (equal local (funcall read-host t nil)))
      (should (equal '("server" "start") started))
      (should-not asked)
      (setq answer t)
      (should (equal local (funcall read-host 'ask nil)))
      (should asked)
      (setq answer nil)
      (should-not (funcall read-host 'ask nil))
      (should-not started)
      (should-not (funcall read-host nil nil))
      (should-not (or started asked))
      (should (equal local (funcall read-host t (list local))))
      (should-not (or started asked)))))

(ert-deftest rexel-test-socket-live-p ()
  "Only a socket with a listener counts; a leftover file does not."
  (let* ((dir (make-temp-file "rexel" t))
         (path (expand-file-name "s" dir))
         (leftover (expand-file-name "leftover" dir)))
    (unwind-protect
        (let ((server (make-network-process :name "rexel-test" :family 'local
                                            :service path :server t :noquery t)))
          (should (rexel--socket-live-p path))
          (delete-process server)
          (should-not (rexel--socket-live-p (expand-file-name "missing" dir)))
          (write-region "" nil leftover)
          (should-not (rexel--socket-live-p leftover)))
      (delete-directory dir t))))

(ert-deftest rexel-test-start-server-needs-program ()
  "The command reports a missing program or a failed start."
  (cl-letf (((symbol-function 'executable-find) #'ignore))
    (should-error (rexel-start-server) :type 'user-error))
  (cl-letf (((symbol-function 'executable-find) (lambda (_) "/bin/rex"))
            ((symbol-function 'call-process) (lambda (&rest _) 1)))
    (should-error (rexel-start-server) :type 'user-error)))

(provide 'rexel-test)
;;; rexel-test.el ends here
