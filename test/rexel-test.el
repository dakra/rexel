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
  "A refused RESUME has the block streamed afresh, a repeated refused HELLO not."
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
            (setq rexel--data-state 'welcome
                  rexel--stream stream
                  rexel--hello-refused t)
            (rexel--on-welcome (unibyte-string #x82 3))
            (should (eq rexel--data-state 'closed))
            (should-not rexel--stream)
            (should-not (process-live-p (websocket-conn ws)))
            (should (equal (and resume '("s")) refreshed))
            (should (eq (null resume)
                        (eq buf (gethash "b" (rexel--conn-blocks conn)))))))))))

(ert-deftest rexel-test-control-welcome-checks-revision ()
  (pcase-dolist (`(,protocol ,revision ,ok)
                 `(("rex.control.v1" ,rexel--api-revision t)
                   ("rex.control.v1" 7 nil)
                   ("rex.control.v1" nil nil)
                   ("other" ,rexel--api-revision nil)))
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
                           ("attic" "https://attic.ts.net" "https://attic.ts.net:7297"
                            "http://10.0.0.2:7297")
                           ("far" "https://far.ts.net" "https://far.ts.net:7297")
                           ("near" "/tmp/near.sock"
                            "https://near.ts.net" "https://near.ts.net:7297"))
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

(ert-deftest rexel-test-endpoints-add-default-port ()
  (pcase-dolist (`(,addresses . ,endpoints)
                 '((("studio.ts.net")
                    "https://studio.ts.net" "https://studio.ts.net:7297")
                   (("studio.ts.net:8443") "https://studio.ts.net:8443")
                   (("https://studio.ts.net")
                    "https://studio.ts.net" "https://studio.ts.net:7297")
                   (("http://10.0.0.2") "http://10.0.0.2" "http://10.0.0.2:7297")
                   (("https://studio.ts.net:443") "https://studio.ts.net:443")
                   (("http://10.0.0.2:7297") "http://10.0.0.2:7297")
                   (("https://studio.ts.net/rex") "https://studio.ts.net/rex")
                   (("[fd7a::1]") "https://[fd7a::1]" "https://[fd7a::1]:7297")
                   (("[fd7a::1]:8443") "https://[fd7a::1]:8443")
                   (("/tmp/rex.sock") "/tmp/rex.sock")
                   (("ws://studio.ts.net"))
                   ;; An endpoint named twice is dialled once.
                   (("studio.ts.net" "https://studio.ts.net:7297" "studio.ts.net/")
                    "https://studio.ts.net" "https://studio.ts.net:7297")))
    (should (equal endpoints (rexel--endpoints addresses)))))

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
         conn (format "{\"type\":\"welcome\",\"protocol\":\"rex.control.v1\",\"api_revision\":%d}"
                      rexel--api-revision)))
      (rexel-test--dial-event (car dialled) 'close)
      (should closed)
      (should (= 2 (length dialled))))))

(ert-deftest rexel-test-dial-starts-at-last-answering-endpoint ()
  (let ((conn (rexel--make-conn :endpoints (list "a" "b" "c") :endpoint "b"))
        dialled closed)
    (rexel-test--with-dials dialled
      (rexel--dial conn)
      (should (equal '("b") (mapcar #'car dialled)))
      (should (equal '("a" "c") (rexel--conn-untried conn)))
      (should (equal '("a" "b" "c") (rexel--conn-endpoints conn)))
      (should-not closed))))

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
             `((type . "welcome") (protocol . "rex.control.v1")
               (api_revision . ,rexel--api-revision)
               (client . ((id . "client:1") (reconnect_token . "tok") (resumed . t))))))
      (should (equal "tok" (rexel--conn-token conn)))
      (should-not (rexel--conn-delay conn))
      (should (equal "session.attach" (alist-get 'method (car sent))))
      (should (equal "s" (alist-get 'session_id (alist-get 'payload (car sent))))))))

(ert-deftest rexel-test-reattach-does-not-repeat-a-write ()
  "A write in flight when the connection dropped is reported, not sent again."
  (rexel-test--with-fake-conn sent
    (let ((conn rexel--conn)
          messages)
      (setf (alist-get "s" (rexel--conn-sessions conn) nil nil #'equal) "work")
      (rexel--pty-out "a")
      (rexel--pty-out "b")
      (setq sent nil)
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest args)
                   (push (apply #'format format args) messages))))
        (rexel--reattach conn))
      (should (string-match-p "may be lost" (car messages)))
      (should-not rexel--out-inflight)
      (should (equal '("session.attach")
                     (mapcar (lambda (object) (alist-get 'method object)) sent)))
      ;; What was typed after it follows the attach.
      (cl-letf (((symbol-function 'rexel--apply-view) #'ignore)
                ((symbol-function 'rexel--sync-size) #'ignore))
        (rexel-test--respond conn (car sent) '((view))))
      (should (equal "b" (rexel-test--sent-data (car sent)))))))

(defun rexel-test--view (bid)
  "Return a session view with the one terminal block BID."
  `((label . "work")
    (windows . [((layers . [((blocks . [((block_id . ,bid)
                                         (creator_name . ,rexel--terminal-creator))]))]))])))

(defun rexel-test--count (method sent)
  "Return how many of the requests SENT call METHOD."
  (seq-count (lambda (object) (equal method (alist-get 'method object))) sent))

(defmacro rexel-test--with-attached-block (conn &rest body)
  "Run BODY with this buffer as block \"block:123456\" of session \"s\" on CONN.
The terminal and the stream's connection are stubbed."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'ghostel--init-buffer) #'ignore)
             ((symbol-function 'rexel-block-mode) #'ignore)
             ((symbol-function 'rexel--open-stream) #'ignore)
             ((symbol-function 'rexel--sync-size) #'ignore))
     (setq rexel--block-id "block:123456")
     (setf (alist-get "s" (rexel--conn-sessions ,conn) nil nil #'equal) "work")
     (puthash rexel--block-id (current-buffer) (rexel--conn-blocks ,conn))
     ,@body))

(ert-deftest rexel-test-reattach-restreams-lost-connect ()
  "A block whose `data.connect' went with the connection is streamed again."
  (rexel-test--with-fake-conn sent
    (let ((conn rexel--conn))
      (rexel-test--with-attached-block conn
        (rexel--reattach conn)
        (should (= 0 (rexel-test--count "data.connect" sent)))
        (rexel-test--respond conn (car sent)
                             `((view . ,(rexel-test--view rexel--block-id))))
        (should (= 1 (rexel-test--count "data.connect" sent)))
        (rexel-test--respond conn (car sent) '((ticket . "AAAA")))
        (should (eq 'welcome rexel--data-state))))))

(ert-deftest rexel-test-reattach-sends-keys-typed-during-outage ()
  "Bytes typed while the connection was down go out once, and the block streams."
  (rexel-test--with-fake-conn sent
    (let ((conn rexel--conn))
      (rexel-test--with-attached-block conn
        (setq rexel--out-pending "a")
        (rexel--reattach conn)
        (rexel-test--respond conn (car sent)
                             `((view . ,(rexel-test--view rexel--block-id))))
        (should (= 1 (rexel-test--count "data.connect" sent)))
        (should (= 1 (rexel-test--count "com.superlogical.terminal.write" sent)))))))

(defun rexel-test--writes (sent)
  "Return the typed text of the write requests in SENT, oldest first."
  (nreverse
   (delq nil (mapcar (lambda (object)
                       (when (equal "com.superlogical.terminal.write"
                                    (alist-get 'method object))
                         (rexel-test--sent-data object)))
                     sent))))

(ert-deftest rexel-test-restream-keeps-held-back-keys ()
  "Bytes waiting for a write's answer survive the block's new stream."
  (rexel-test--with-fake-conn sent
    (let ((conn rexel--conn))
      (rexel-test--with-attached-block conn
        (rexel--reattach conn)
        (let ((attach (car sent)))
          (rexel--pty-out "a")
          (let ((write (car sent)))
            (rexel--pty-out "b")
            (rexel-test--respond conn attach
                                 `((view . ,(rexel-test--view rexel--block-id))))
            (rexel-test--respond conn write)
            (should (equal '("a" "b") (rexel-test--writes sent)))))))))

(ert-deftest rexel-test-reattach-clears-size-in-flight ()
  "A size request whose answer the dropped connection lost holds nothing back."
  (rexel-test--with-fake-conn sent
    (setf (alist-get "s" (rexel--conn-sessions rexel--conn) nil nil #'equal) "work")
    (setq rexel--size-inflight rexel--conn)
    (rexel--reattach rexel--conn)
    (should-not rexel--size-inflight)))

(ert-deftest rexel-test-restream-keeps-write-in-flight ()
  "A write sent before the block's new stream still holds back the next."
  (rexel-test--with-fake-conn sent
    (let ((conn rexel--conn))
      (rexel-test--with-attached-block conn
        (setq rexel--out-pending "a")
        (rexel--reattach conn)
        (rexel-test--respond conn (car sent)
                             `((view . ,(rexel-test--view rexel--block-id))))
        (rexel--pty-out "b")
        (should (equal '("a") (rexel-test--writes sent)))))))

(ert-deftest rexel-test-attach-on-another-connection-drops-typed-bytes ()
  "A buffer taken over from another connection starts with nothing to send."
  (rexel-test--with-fake-conn sent
    (let ((conn rexel--conn)
          (other (rexel--make-conn :endpoints (list "other"))))
      (rexel-test--with-attached-block conn
        (setq rexel--out-pending "a"
              rexel--out-inflight other
              rexel--conn other)
        (rexel--attach-block-buffer conn "s" rexel--block-id (current-buffer))
        (should-not rexel--out-pending)
        (should-not rexel--out-inflight)))))

(ert-deftest rexel-test-reattach-leaves-other-connections-connect ()
  "A reconnect leaves the `data.connect' of another connection in flight."
  (rexel-test--with-fake-conn sent
    (let ((conn rexel--conn)
          (other (rexel--make-conn :endpoints (list "other") :client-id t
                                   :ws (websocket-inner-create :conn nil :url "o"
                                                               :ready-state 'closed))))
      (rexel-test--with-attached-block conn
        (setf (alist-get "s" (rexel--conn-sessions other) nil nil #'equal) "work")
        (rexel--attach-block-buffer conn "s" rexel--block-id (current-buffer))
        (rexel--reattach other)
        (should-not rexel--data-state)))))

(ert-deftest rexel-test-refused-hello-streams-again-once ()
  "A refused first HELLO gets one new ticket, and a second refusal is final."
  (rexel-test--with-fake-conn sent
    (let ((conn rexel--conn)
          (refreshed 0)
          (hello-refused (lambda ()
                           (setq rexel--data-state 'welcome
                                 rexel--stream (rexel--make-stream))
                           (rexel--on-welcome (unibyte-string #x82 1)))))
      (rexel-test--with-attached-block conn
        (cl-letf (((symbol-function 'rexel--schedule-view-refresh)
                   (lambda (&rest _) (cl-incf refreshed)))
                  ((symbol-function 'ghostel--write-vt) #'ignore)
                  ((symbol-function 'ghostel--invalidate) #'ignore)
                  ((symbol-function 'message) #'ignore))
          (funcall hello-refused)
          (should (= 1 refreshed))
          (should (eq 'closed rexel--data-state))
          (puthash rexel--block-id (current-buffer) (rexel--conn-blocks conn))
          (funcall hello-refused)
          (should (= 1 refreshed))
          (should (eq 'closed rexel--data-state))
          ;; A stream that got through earns the retry back.
          (setq rexel--data-state 'welcome
                rexel--stream (rexel--make-stream))
          (rexel--on-welcome (rexel-test--welcome (make-string 16 ?t)))
          (funcall hello-refused)
          (should (= 2 refreshed)))))))

(ert-deftest rexel-test-reattach-leaves-running-streams ()
  "A block with a stream keeps it through a reconnect of the control connection."
  (dolist (state '(welcome snapshot live))
    (rexel-test--with-fake-conn sent
      (let ((conn rexel--conn))
        (rexel-test--with-attached-block conn
          (setq rexel--data-state state)
          (rexel--reattach conn)
          (rexel-test--respond conn (car sent)
                               `((view . ,(rexel-test--view rexel--block-id))))
          (should (= 0 (rexel-test--count "data.connect" sent)))
          (should (eq state rexel--data-state)))))))

(ert-deftest rexel-test-view-refresh-keeps-first-connect ()
  "A view applied during a healthy first `data.connect' asks for no second one."
  (rexel-test--with-fake-conn sent
    (let ((conn rexel--conn))
      (rexel-test--with-attached-block conn
        (rexel--attach-block-buffer conn "s" rexel--block-id (current-buffer))
        (should (= 1 (rexel-test--count "data.connect" sent)))
        (rexel--apply-view conn "s" (rexel-test--view rexel--block-id))
        (should (= 1 (rexel-test--count "data.connect" sent)))))))

(ert-deftest rexel-test-view-of-detached-session-is-dropped ()
  (rexel-test--with-fake-conn sent
    (let ((conn rexel--conn)
          (applied 0))
      (setf (alist-get "t" (rexel--conn-sessions conn) nil nil #'equal) "work")
      (cl-letf (((symbol-function 'rexel--apply-view)
                 (lambda (&rest _) (cl-incf applied))))
        (rexel--refresh-view conn "t")
        (rexel-test--respond conn (car sent) '((revision . 1)))
        (should (= 1 applied))
        (rexel--refresh-view conn "t")
        (rexel--forget-session conn "t")
        (rexel-test--respond conn (car sent) '((revision . 2)))
        (should (= 1 applied))))))

(ert-deftest rexel-test-sync-layout-view-of-detached-session-is-dropped ()
  (rexel-test--with-fake-conn sent
    (let ((conn rexel--conn)
          (applied 0))
      (setf (alist-get "t" (rexel--conn-sessions conn) nil nil #'equal) "work")
      (cl-letf (((symbol-function 'rexel--read-session)
                 (lambda () (cons conn "t")))
                ((symbol-function 'rexel--apply-view)
                 (lambda (&rest _) (cl-incf applied))))
        (rexel-sync-layout)
        (rexel--forget-session conn "t")
        (rexel-test--respond conn (car sent) '((revision . 1)))
        (should (= 0 applied))))))

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

(defun rexel-test--respond (conn object &optional result err)
  "Answer the request OBJECT on CONN with RESULT or the error ERR."
  (funcall (gethash (alist-get 'id object) (rexel--conn-pending conn)) result err))

(ert-deftest rexel-test-pty-out-during-send-waits-for-the-answer ()
  "Bytes typed while a write is being sent follow its answer."
  (rexel-test--with-fake-conn sent
    (let ((buf (current-buffer))
          (nested nil))
      (cl-letf (((symbol-function 'rexel--send)
                 (lambda (_conn object)
                   (push object sent)
                   (unless nested
                     (setq nested t)
                     (with-current-buffer buf (rexel--pty-out "b"))))))
        (rexel--pty-out "a"))
      (should (= 1 (length sent)))
      (should (equal "a" (rexel-test--sent-data (car sent))))
      (should (equal "b" rexel--out-pending))
      (rexel-test--respond rexel--conn (car sent))
      (should (= 2 (length sent)))
      (should (equal "b" (rexel-test--sent-data (car sent)))))))

(ert-deftest rexel-test-answer-during-send-clears-the-mark ()
  "A write answered while it is being sent leaves no write in flight."
  (rexel-test--with-fake-conn sent
    (let ((conn rexel--conn))
      (cl-letf (((symbol-function 'rexel--send)
                 (lambda (_conn object)
                   (push object sent)
                   (rexel-test--respond conn object))))
        (rexel--pty-out "a"))
      (should-not rexel--out-inflight))))

(ert-deftest rexel-test-pty-out-during-failed-send-goes-on ()
  "A failed send leaves no write in flight, and bytes typed during it follow."
  (rexel-test--with-fake-conn sent
    (let ((buf (current-buffer)))
      (cl-letf (((symbol-function 'rexel--send)
                 (lambda (&rest _)
                   (with-current-buffer buf (rexel--pty-out "b"))
                   (error "Boom"))))
        (rexel--pty-out "a"))
      (should-not sent)
      (should-not rexel--out-inflight)
      (rexel--pty-out "c")
      (should (equal "bc" (rexel-test--sent-data (car sent)))))))

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

(ert-deftest rexel-test-resize-during-send-waits-for-the-answer ()
  "A size requested while another is being sent follows its answer."
  (rexel-test--with-fake-conn sent
    (setq rexel--data-state 'live)
    (let ((buf (current-buffer))
          (nested nil))
      (cl-letf (((symbol-function 'rexel--send)
                 (lambda (_conn object)
                   (push object sent)
                   (unless nested
                     (setq nested t)
                     (with-current-buffer buf (rexel--resize 30 100))))))
        (rexel--resize 24 80))
      (should (= 1 (length sent)))
      (rexel-test--respond rexel--conn (car sent))
      (should (= 2 (length sent)))
      (should (equal '((columns . 100) (rows . 30))
                     (alist-get 'args (alist-get 'payload (car sent))))))))

(ert-deftest rexel-test-size-answer-during-send-clears-the-mark ()
  "A size answered while it is being sent leaves no request in flight."
  (rexel-test--with-fake-conn sent
    (setq rexel--data-state 'live)
    (let ((conn rexel--conn))
      (cl-letf (((symbol-function 'rexel--send)
                 (lambda (_conn object)
                   (push object sent)
                   (rexel-test--respond conn object))))
        (rexel--resize 24 80))
      (should-not rexel--size-inflight))))

(ert-deftest rexel-test-size-failed-send-clears-the-mark ()
  "A size that cannot be sent signals and leaves no request in flight."
  (rexel-test--with-fake-conn sent
    (setq rexel--data-state 'live)
    (cl-letf (((symbol-function 'rexel--send)
               (lambda (&rest _) (error "Boom"))))
      (should-error (rexel--resize 24 80)))
    (should-not rexel--size-inflight)
    (rexel--resize 30 100)
    (should (= 1 (length sent)))))

(ert-deftest rexel-test-restream-keeps-size-in-flight ()
  "A size request outlives the block's new stream, and sends nothing after it."
  (rexel-test--with-fake-conn sent
    (let ((conn rexel--conn))
      (rexel-test--with-attached-block conn
        (setq rexel--data-state 'live)
        (rexel--resize 24 80)
        (let ((resize (car sent)))
          (rexel--attach-block-buffer conn "s" rexel--block-id (current-buffer))
          (should (eq conn rexel--size-inflight))
          (setq sent nil)
          (rexel-test--respond conn resize)
          (should-not rexel--size-inflight)
          (should-not sent))))))

(ert-deftest rexel-test-restream-during-take-sends-no-size ()
  "A take answered after the block's new stream began sends no resize."
  (rexel-test--with-fake-conn sent
    (let ((conn rexel--conn))
      (rexel-test--with-attached-block conn
        (setq rexel--data-state 'live)
        (rexel--resize 24 80 t)
        (let ((take (car sent)))
          (rexel--attach-block-buffer conn "s" rexel--block-id (current-buffer))
          (setq sent nil)
          (rexel-test--respond conn take)
          (should-not rexel--size-inflight)
          (should-not sent))))))

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
                                     :on-close #'ignore
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

(defun rexel-test--expire (write)
  "Run the timer of WRITE as if it were past its deadline."
  (setf (rexel--write-deadline write) 0)
  (timer-event-handler (rexel--write-timer write)))

(ert-deftest rexel-test-write-stall-ends-innermost-write ()
  "A write held up by one to another socket is not what stalled."
  (let (stalled
        (rexel--writes-overdue nil))
    (rexel--watched-write
     'outer (lambda () (push 'outer stalled))
     (lambda ()
       (let ((outer (car rexel--writes)))
         (rexel--watched-write
          'inner (lambda () (push 'inner stalled))
          (lambda ()
            (rexel-test--expire outer)
            (should-not stalled)
            (should (equal (list outer) rexel--writes-overdue))
            (should (memq (rexel--write-timer outer) timer-list))
            (rexel-test--expire (car rexel--writes))
            (should (equal '(inner) stalled))))
         (should (equal (list outer) rexel--writes-overdue))
         ;; Once the inner write is gone the outer one is the innermost.
         (rexel-test--expire outer)
         (should (equal '(outer inner) stalled)))))
    (should-not rexel--writes-overdue)
    (should-not rexel--writes)
    (should-not (seq-find (lambda (timer)
                            (eq (timer--function timer) #'rexel--write-stalled))
                          timer-list))))

(ert-deftest rexel-test-write-queued-behind-another-is-not-stalled ()
  "A write to a busy socket waits for the earlier write, whose time counts."
  (let (stalled
        (rexel--writes-overdue nil))
    (rexel--watched-write
     'socket (lambda () (push 'earlier stalled))
     (lambda ()
       (let ((earlier (car rexel--writes)))
         (rexel--watched-write
          'other #'ignore
          (lambda ()
            (rexel--watched-write
             'socket (lambda () (push 'queued stalled))
             (lambda ()
               (let ((queued (car rexel--writes)))
                 (rexel-test--expire queued)
                 (should-not stalled)
                 (should-not rexel--writes-overdue)
                 (rexel-test--expire earlier)
                 (should (equal '(earlier) stalled))))
             0))
          0)))
     0)))

(ert-deftest rexel-test-new-writes-do-not-buy-a-stalled-socket-time ()
  "A queued write adds its bytes' time to the earliest write, nothing more."
  (rexel--watched-write
   'socket #'ignore
   (lambda ()
     (let* ((earliest (car rexel--writes))
            (deadline (rexel--write-deadline earliest)))
       (rexel--watched-write 'socket #'ignore #'ignore 100)
       (should (= deadline (rexel--write-deadline earliest)))
       (rexel--watched-write 'other #'ignore #'ignore (* 10 rexel--write-rate))
       (should (= deadline (rexel--write-deadline earliest)))
       (rexel--watched-write 'socket #'ignore #'ignore (* 10 rexel--write-rate))
       (should (= (+ deadline 10) (rexel--write-deadline earliest)))))
   0))

(ert-deftest rexel-test-write-in-time-is-left-alone ()
  "A tick before the deadline, moved by bytes or a handler, gives up nothing."
  (let (stalled)
    (rexel--watched-write
     'socket (lambda () (push 'write stalled))
     (lambda ()
       (let ((write (car rexel--writes)))
         (timer-event-handler (rexel--write-timer write))
         (should-not stalled)
         (setf (rexel--write-deadline write) (- (float-time) 1))
         (rexel--watched-write 'socket #'ignore
                               (lambda ()
                                 (timer-event-handler (rexel--write-timer write))
                                 (should-not stalled))
                               (* 10 rexel--write-rate))
         (setf (rexel--write-deadline write) (+ (float-time) 0.1))
         (rexel--unwatched (lambda () (sleep-for 0.15)))
         (timer-event-handler (rexel--write-timer write))
         (should-not stalled)))
     0)))

(ert-deftest rexel-test-prompt-counts-before-the-handler-writes ()
  "A write of the handler itself finds the prompt's time counted."
  (rexel--watched-write
   'socket #'ignore
   (lambda ()
     (let* ((write (car rexel--writes))
            (deadline (rexel--write-deadline write)))
       (rexel--unwatched
        (lambda ()
          (sleep-for 0.03)
          (rexel--watched-write
           'socket #'ignore
           (lambda ()
             (should (<= 0.02 (- (rexel--write-deadline write) deadline))))
           0)
          ;; Counted once: the time before the write does not come back.
          (setq deadline (rexel--write-deadline write))))
       (should (< (- (rexel--write-deadline write) deadline) 0.02))))
   0))

(ert-deftest rexel-test-a-stall-under-a-handler-has-a-bound ()
  "While the handler's own write waits, no further time is counted."
  (rexel--watched-write
   'socket #'ignore
   (lambda ()
     (let ((write (car rexel--writes)))
       (rexel--unwatched
        (lambda ()
          (rexel--watched-write
           'socket #'ignore
           (lambda ()
             (let ((deadline (rexel--write-deadline write)))
               (sleep-for 0.02)
               (rexel--write-stalled (car rexel--writes))
               (should (= deadline (rexel--write-deadline write)))))
           0)))))
   0))

(ert-deftest rexel-test-overdue-set-below-an-outer-handler ()
  "A handler outside a write does not excuse it from holding uploads back."
  (let ((rexel--writes-overdue nil))
    (rexel--unwatched
     (lambda ()
       (rexel--watched-write
        'held #'ignore
        (lambda ()
          (let ((held (car rexel--writes)))
            (rexel--watched-write
             'other #'ignore
             (lambda ()
               (rexel-test--expire held)
               (should (equal (list held) rexel--writes-overdue)))
             0)))
        0)))))

(ert-deftest rexel-test-stall-is-looked-at-every-second ()
  (rexel--watched-write
   nil #'ignore
   (lambda ()
     (should (= 1 (timer--repeat-delay (rexel--write-timer (car rexel--writes))))))))

(ert-deftest rexel-test-overdue-writes-are-held-separately ()
  "One overdue write ending does not release another."
  (let ((rexel--writes-overdue nil))
    (rexel--watched-write
     'first #'ignore
     (lambda ()
       (let ((first (car rexel--writes)))
         (rexel--watched-write
          'second #'ignore
          (lambda ()
            (let ((second (car rexel--writes)))
              (rexel--watched-write
               'third #'ignore
               (lambda ()
                 (rexel-test--expire first)
                 (rexel-test--expire second)
                 (rexel-test--expire first)
                 (should (= 2 (length rexel--writes-overdue))))
               0)))
          0)
         (should (equal (list first) rexel--writes-overdue))))
     0)
    (should-not rexel--writes-overdue)))

(ert-deftest rexel-test-write-timer-goes-with-its-write ()
  "A write that ends, or signals, leaves no timer."
  (should (eq 'done (rexel--watched-write nil #'ignore (lambda () 'done))))
  (should-error (rexel--watched-write nil #'ignore (lambda () (error "Boom"))))
  (should-not rexel--writes)
  (should-not (seq-find (lambda (timer)
                          (eq (timer--function timer) #'rexel--write-stalled))
                        timer-list)))

(ert-deftest rexel-test-write-time-grows-with-its-size ()
  (rexel--watched-write
   nil #'ignore
   (lambda ()
     (should (< -11 (timer-until (rexel--write-timer (car rexel--writes))
                                 (current-time))
                -9))))
  (rexel--watched-write
   nil #'ignore
   (lambda ()
     (should (< -75 (timer-until (rexel--write-timer (car rexel--writes))
                                 (current-time))
                -70)))
   (* 1024 1024)))

(ert-deftest rexel-test-send-tells-the-size-of-its-write ()
  (let ((conn (rexel--make-conn :ws (rexel-test--ws)))
        seconds)
    (unwind-protect
        (cl-letf (((symbol-function 'websocket-send-text)
                   (lambda (_ws _text)
                     (setq seconds (- (timer-until
                                       (rexel--write-timer (car rexel--writes))
                                       (current-time)))))))
          ;; Three bytes each.
          (rexel--send conn `((data . ,(make-string 54614 ?日))))
          (should (< 19 seconds 21)))
      (rexel-test--ws-delete (rexel--conn-ws conn)))))

(ert-deftest rexel-test-unwatched-holds-every-write ()
  "No write counts as stalled while a handler runs, and gets its time back."
  (let (stalled
        (rexel--writes-overdue nil))
    (rexel--watched-write
     'outer (lambda () (push 'outer stalled))
     (lambda ()
       (rexel--watched-write
        'inner (lambda () (push 'inner stalled))
        (lambda ()
          (let* ((writes rexel--writes)
                 (deadlines (mapcar #'rexel--write-deadline writes))
                 (rexel--writes-overdue (list (car writes))))
            (rexel--unwatched
             (lambda ()
               (should-not (rexel--write-blocking))
               (should-not rexel--writes-overdue)
               (dolist (write writes)
                 (timer-event-handler (rexel--write-timer write)))
               (should-not rexel--writes-overdue)
               (sleep-for 0.02)))
            (should-not stalled)
            (should (equal writes rexel--writes))
            (should (equal (list (car writes)) rexel--writes-overdue))
            (cl-mapc (lambda (write deadline)
                       (should (> (rexel--write-deadline write) (+ deadline 0.01))))
                     writes deadlines)
            (rexel-test--expire (car writes))
            (should (equal '(inner) stalled)))))))))

(ert-deftest rexel-test-unwatched-gives-time-back-after-an-error ()
  (rexel--watched-write
   nil #'ignore
   (lambda ()
     (let* ((write (car rexel--writes))
            (deadline (rexel--write-deadline write)))
       (should-error (rexel--unwatched (lambda ()
                                         (sleep-for 0.02)
                                         (error "Boom"))))
       (should (> (rexel--write-deadline write) deadline))))))

(ert-deftest rexel-test-big-write-outlives-the-filter-it-runs-in ()
  "A filter on a socket does not cut a write sent from its message handler."
  (let (stalled)
    (rexel--watched-write
     'socket (lambda () (push 'filter stalled))
     (lambda ()
       (let ((filter (car rexel--writes)))
         (rexel--unwatched
          (lambda ()
            (rexel--watched-write
             'socket (lambda () (push 'write stalled))
             (lambda ()
               (should (> (rexel--write-deadline (car rexel--writes))
                          (+ (float-time) 70)))
               (rexel-test--expire filter)
               (should-not stalled)
               (rexel-test--expire (car rexel--writes))
               (should (equal '(write) stalled)))
             (* 1024 1024)))))))))

(ert-deftest rexel-test-pong-queues-behind-a-write ()
  "A filter nested in a write to its socket waits for that write."
  (let (stalled)
    (rexel--watched-write
     'socket (lambda () (push 'write stalled))
     (lambda ()
       (let ((write (car rexel--writes)))
         (rexel--watched-write
          'socket (lambda () (push 'filter stalled))
          (lambda ()
            (should (eq write (rexel--write-blocking)))
            (rexel-test--expire (car rexel--writes))
            (should-not stalled)))))
     0)))

(ert-deftest rexel-test-write-blocking-is-the-earliest-to-the-socket ()
  (rexel--watched-write
   'socket #'ignore
   (lambda ()
     (let ((earliest (car rexel--writes)))
       (rexel--watched-write
        'socket #'ignore
        (lambda ()
          (rexel--watched-write
           'socket #'ignore
           (lambda ()
             (should (eq earliest (rexel--write-blocking))))
           0))
        0)))
   0))

(ert-deftest rexel-test-nested-handlers-count-their-time-once ()
  (rexel--watched-write
   nil #'ignore
   (lambda ()
     (let* ((write (car rexel--writes))
            (deadline (rexel--write-deadline write))
            (began (float-time)))
       (rexel--unwatched
        (lambda ()
          (rexel--unwatched (lambda () (sleep-for 0.02)))
          (rexel--unwatched (lambda () (sleep-for 0.02)))))
       (should (<= 0.03 (- (rexel--write-deadline write) deadline)
                   (+ 0.01 (- (float-time) began))))))))

(ert-deftest rexel-test-write-under-handler-holds-no-uploads-back ()
  "A write waiting for a prompt does not stop the chunks of other sockets."
  (let ((rexel--writes-overdue nil))
    (rexel--watched-write
     'filter #'ignore
     (lambda ()
       (let ((filter (car rexel--writes)))
         (rexel--unwatched
          (lambda ()
            (rexel--watched-write
             'upload #'ignore
             (lambda ()
               (rexel-test--expire filter)
               (should-not rexel--writes-overdue))))))))))

(ert-deftest rexel-test-write-in-handler-queues-behind-outer-write ()
  "A handler's write to a busy socket does not take the blame for it."
  (let (stalled)
    (rexel--watched-write
     'socket (lambda () (push 'earlier stalled))
     (lambda ()
       (let ((earlier (car rexel--writes)))
         (rexel--unwatched
          (lambda ()
            (rexel--watched-write
             'socket (lambda () (push 'queued stalled))
             (lambda ()
               (rexel-test--expire (car rexel--writes))
               (should-not stalled)
               (rexel-test--expire earlier)
               (should (equal '(earlier) stalled)))
             0)))))
     0)))

(ert-deftest rexel-test-ws-open-runs-messages-unwatched ()
  "A message handler does not count as a write of the filter it runs in."
  (let* ((socket (make-temp-name
                  (expand-file-name "rexel-test" temporary-file-directory)))
         (server (make-network-process :name "rexel-test-server" :server t
                                       :family 'local :service socket :noquery t))
         stack)
    (unwind-protect
        (let ((ws (rexel--ws-open socket "/api/data" "p" #'ignore
                                  (lambda (_ws _frame)
                                    (setq stack (car rexel--writes)))
                                  #'ignore)))
          (cl-letf (((symbol-function 'websocket-outer-filter)
                     (lambda (ws _output)
                       (funcall (websocket-on-message ws) ws nil))))
            (rexel--ws-filter (websocket-conn ws) "x"))
          (should (eq 'handler stack))
          (delete-process (websocket-conn ws)))
      (delete-process server)
      (when (file-exists-p socket)
        (delete-file socket)))))

(ert-deftest rexel-test-send-stall-deletes-connection ()
  "A control write that takes too long closes its connection."
  (let* ((ws (rexel-test--ws))
         (conn (rexel--make-conn :ws ws)))
    (unwind-protect
        (cl-letf (((symbol-function 'websocket-send-text)
                   (lambda (_ws _text)
                     (rexel-test--expire (car rexel--writes)))))
          (rexel--send conn '((type . "hello")))
          (should-not (process-live-p (websocket-conn ws))))
      (rexel-test--ws-delete ws))))

(ert-deftest rexel-test-filter-error-on-live-connection-surfaces ()
  (let ((ws (rexel-test--ws)))
    (unwind-protect
        (cl-letf (((symbol-function 'websocket-outer-filter)
                   (lambda (_ws _output) (error "Boom"))))
          (should-error (rexel--ws-filter (websocket-conn ws) "x")))
      (rexel-test--ws-delete ws))))

(ert-deftest rexel-test-probe-skips-a-socket-being-written ()
  "A ping would queue behind the write, which is watched itself."
  (let ((ws (rexel-test--ws))
        pings)
    (unwind-protect
        (cl-letf (((symbol-function 'websocket-send)
                   (lambda (&rest _) (cl-incf pings))))
          (setq pings 0)
          (rexel--watched-write (websocket-conn ws) #'ignore
                                (lambda () (rexel--probe ws))
                                0)
          (should (= 0 pings))
          (rexel--probe ws)
          (should (= 1 pings)))
      (rexel-test--ws-delete ws))))

(ert-deftest rexel-test-filter-is-no-write ()
  "A filter does not count as a write, so a ping can go out from under one."
  (let ((ws (rexel-test--ws))
        filter ping)
    (unwind-protect
        (cl-letf (((symbol-function 'websocket-outer-filter)
                   (lambda (ws _output)
                     (setq filter (car rexel--writes))
                     (rexel--probe ws)))
                  ((symbol-function 'websocket-send)
                   (lambda (&rest _) (setq ping (car rexel--writes)))))
          (rexel--ws-filter (websocket-conn ws) "x")
          (should-not (rexel--write-size filter))
          (should ping)
          (should (rexel--write-size ping)))
      (rexel-test--ws-delete ws))))

(ert-deftest rexel-test-probe-stall-deletes-connection ()
  "A ping that finds the socket full closes its connection."
  (let ((ws (rexel-test--ws))
        watched)
    (unwind-protect
        (cl-letf (((symbol-function 'websocket-send)
                   (lambda (_ws _frame)
                     (setq watched (car rexel--writes))
                     (when watched
                       (rexel-test--expire watched)))))
          (rexel--probe ws)
          (should watched)
          (should-not (process-live-p (websocket-conn ws))))
      (rexel-test--ws-delete ws))))

(ert-deftest rexel-test-pong-stall-deletes-connection ()
  "The pong websocket.el sends from the filter is watched too."
  (let ((ws (rexel-test--ws)))
    (unwind-protect
        (cl-letf (((symbol-function 'websocket-outer-filter)
                   (lambda (_ws _output)
                     (rexel-test--expire (car rexel--writes))
                     (error "Output file descriptor is closed"))))
          (rexel--ws-filter (websocket-conn ws) "x")
          (should-not (process-live-p (websocket-conn ws))))
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

(defmacro rexel-test--with-uploads (&rest body)
  "Run BODY in a block buffer of a fake connection to the host \"studio\".
The variable `sent' collects the requests, `opened' a
\(WS ON-OPEN ON-MESSAGE ON-CLOSE) per data connection and `frames' the
\(WS OPCODE PAYLOAD) of each frame sent, all newest first."
  (declare (indent 0))
  `(let (opened frames)
     (rexel-test--with-fake-conn sent
       (setf (rexel--conn-host rexel--conn) "studio")
       (unwind-protect
           (cl-letf (((symbol-function 'rexel--ws-open)
                      (lambda (_endpoint _path _protocol &rest callbacks)
                        (let ((ws (rexel-test--ws)))
                          (push (cons ws callbacks) opened)
                          ws)))
                     ((symbol-function 'websocket-send)
                      (lambda (ws frame)
                        (push (list ws (websocket-frame-opcode frame)
                                    (websocket-frame-payload frame))
                              frames)))
                     ;; File chunks bypass `websocket-send'.
                     ((symbol-function 'process-send-string)
                      (lambda (process string)
                        (let ((frame (websocket-read-frame string)))
                          (should (= (length string)
                                     (websocket-frame-length frame)))
                          (push (list (process-get process :websocket)
                                      (websocket-frame-opcode frame)
                                      (websocket-frame-payload frame))
                                frames)))))
             ,@body)
         (dolist (open opened)
           (rexel-test--ws-delete (car open)))))))

(defun rexel-test--upload-file (size)
  "Return a new temporary file of SIZE bytes."
  (let ((coding-system-for-write 'binary))
    (make-temp-file "rexel-upload" nil nil
                    (apply #'unibyte-string
                           (mapcar (lambda (i) (% i 251))
                                   (number-sequence 0 (1- size)))))))

(defun rexel-test--run-upload-timers ()
  "Run the pending upload timers until none is left."
  (let (timer)
    (while (setq timer (seq-find
                        (lambda (timer)
                          (memq (timer--function timer)
                                '(rexel--upload-next rexel--upload-send-chunk)))
                        timer-list))
      (timer-event-handler timer))))

(defun rexel-test--upload-args (object)
  "Return the arguments of the `upload_begin' request OBJECT."
  (alist-get 'args (alist-get 'payload object)))

(defun rexel-test--upload-ticket (conn object)
  "Answer the `upload_begin' request OBJECT on CONN with a ticket."
  (rexel-test--respond
   conn object `((ticket . ,(base64url-encode-string (make-string 16 7) t))
                 (expires_in_ms . 60000))))

(defun rexel-test--upload-welcome (open)
  "Open the data connection OPEN and welcome its HELLO."
  (pcase-let ((`(,ws ,on-open ,on-message ,_) open))
    (funcall on-open ws)
    (funcall on-message ws
             (make-websocket-frame
              :opcode 'binary :completep t
              :payload (concat (unibyte-string #x81) (make-string 31 0))))))

(defun rexel-test--upload-close (open status)
  "Close the data connection OPEN with a close frame of STATUS.
Without STATUS the connection just drops."
  (pcase-let ((`(,ws ,_ ,_ ,on-close) open))
    (when status
      (rexel--upload-filter
       (websocket-conn ws)
       (unibyte-string #x88 2 (ash status -8) (logand status 255))))
    (funcall on-close ws)))

(defun rexel-test--upload-payloads (frames ws)
  "Return the payloads of the binary FRAMES sent on WS, oldest first."
  (let (payloads)
    (pcase-dolist (`(,to ,opcode ,payload) frames)
      (when (and (eq to ws) (eq opcode 'binary))
        (push payload payloads)))
    payloads))

(ert-deftest rexel-test-send-files-local-server-pastes-paths ()
  (rexel-test--with-fake-conn sent
    (let (pasted)
      (cl-letf (((symbol-function 'ghostel-paste-string)
                 (lambda (string) (push string pasted))))
        (rexel--send-files '("/tmp/a b.png" "/tmp/c.txt")))
      (should (equal pasted (list (concat (shell-quote-argument "/tmp/a b.png")
                                          " /tmp/c.txt "))))
      (should-not sent)
      (should-not rexel--upload-queue))))

(ert-deftest rexel-test-send-files-local-server-uploads-remote-files ()
  (let ((file (rexel-test--upload-file 10))
        pasted)
    (unwind-protect
        (rexel-test--with-uploads
          (setf (rexel--conn-host rexel--conn) nil)
          (cl-letf (((symbol-function 'ghostel-paste-string)
                     (lambda (string) (push string pasted)))
                    ((symbol-function 'file-remote-p)
                     (lambda (name &rest _) (equal name file))))
            (should-error (rexel--send-files (list "/tmp" file))
                          :type 'user-error)
            (rexel--send-files (list file)))
          (should-not pasted)
          (should (equal (list file) rexel--upload-queue)))
      (delete-file file))))

(ert-deftest rexel-test-send-files-refuses-directories ()
  (rexel-test--with-uploads
    (should-error (rexel--send-files (list temporary-file-directory))
                  :type 'user-error)
    (should-not rexel--upload-queue)))

(ert-deftest rexel-test-upload-begin-payloads ()
  (let ((a (rexel-test--upload-file 3))
        (b (rexel-test--upload-file 5))
        (case-fold-search nil))
    (unwind-protect
        (rexel-test--with-uploads
          (rexel--send-files (list a))
          (rexel-test--run-upload-timers)
          (should (= 1 (length sent)))
          (let ((payload (alist-get 'payload (car sent)))
                (args (rexel-test--upload-args (car sent))))
            (should (equal "com.superlogical.terminal.upload_begin"
                           (alist-get 'method (car sent))))
            (should (equal '("s" "b") (list (alist-get 'session_id payload)
                                            (alist-get 'block_id payload))))
            (should (equal (file-name-nondirectory a) (alist-get 'name args)))
            (should (= 3 (alist-get 'size args)))
            (should (string-match-p
                     (concat "\\`upload:[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-4[0-9a-f]\\{3\\}"
                             "-[89ab][0-9a-f]\\{3\\}-[0-9a-f]\\{12\\}\\'")
                     (alist-get 'ref args)))
            (should-not (or (assq 'batch args) (assq 'index args)
                            (assq 'count args))))
          (rexel-cancel-upload)
          (setq sent nil)
          (rexel--send-files (list a b))
          (rexel-test--run-upload-timers)
          (let ((args (mapcar #'rexel-test--upload-args (reverse sent))))
            (should (equal '(3 5) (mapcar (lambda (a) (alist-get 'size a)) args)))
            (should (equal '(0 1) (mapcar (lambda (a) (alist-get 'index a)) args)))
            (should (equal '(2 2) (mapcar (lambda (a) (alist-get 'count a)) args)))
            (should (stringp (alist-get 'batch (car args))))
            (should (equal (alist-get 'batch (car args))
                           (alist-get 'batch (cadr args))))
            (should-not (equal (alist-get 'ref (car args))
                               (alist-get 'ref (cadr args))))))
      (delete-file a)
      (delete-file b))))

(ert-deftest rexel-test-upload-sends-chunks-then-end-marker ()
  (let ((file (rexel-test--upload-file 10000)))
    (unwind-protect
        (rexel-test--with-uploads
          (rexel--send-files (list file))
          (rexel-test--run-upload-timers)
          (rexel-test--upload-ticket rexel--conn (car sent))
          (let* ((open (car opened))
                 (ws (car open)))
            (should (eq #'rexel--upload-filter
                        (process-filter (websocket-conn ws))))
            (rexel-test--upload-welcome open)
            (rexel-test--run-upload-timers)
            (let ((payloads (rexel-test--upload-payloads frames ws)))
              (should (equal (concat "\1" (make-string 16 7)) (car payloads)))
              (should (equal '(4096 4096 1808 0)
                             (mapcar #'length (cdr payloads))))
              (should (equal (apply #'concat (cdr payloads))
                             (with-temp-buffer
                               (set-buffer-multibyte nil)
                               (insert-file-contents-literally file)
                               (buffer-string)))))
            (should (eq 'ending (rexel--upload-state (car rexel--uploads))))
            (rexel-test--upload-close open 1000)
            (should-not rexel--uploads)))
      (delete-file file))))

(ert-deftest rexel-test-upload-pauses-once-per-slice ()
  "Once per slice the next chunk is not due at once; a sent chunk shows life."
  (let ((file (rexel-test--upload-file 10000))
        (rexel--upload-slice 60))
    (unwind-protect
        (rexel-test--with-uploads
          (rexel--send-files (list file))
          (rexel-test--run-upload-timers)
          (rexel-test--upload-ticket rexel--conn (car sent))
          (let ((process (websocket-conn (caar opened)))
                delays)
            (process-put process :last-read 0)
            (cl-letf* ((run-at-time (symbol-function 'run-at-time))
                       ((symbol-function 'run-at-time)
                        (lambda (time repeat function &rest args)
                          (when (eq function #'rexel--upload-send-chunk)
                            (push time delays))
                          (apply run-at-time time repeat function args))))
              (rexel-test--upload-welcome (car opened))
              (rexel-test--run-upload-timers))
            (should (equal '(0.001 0 0) (reverse delays)))
            (should (> (process-get process :last-read) 0))
            (let ((rexel--upload-slice 0))
              (should (= 0.001 (rexel--upload-pause process))))))
      (delete-file file))))

(ert-deftest rexel-test-upload-fails-when-connection-stalls ()
  "A chunk the connection does not take in time fails the batch, once."
  (let ((file (rexel-test--upload-file 10000))
        (live t)
        size messages)
    (unwind-protect
        (rexel-test--with-uploads
          (rexel--send-files (list file))
          (rexel-test--run-upload-timers)
          (rexel-test--upload-ticket rexel--conn (car sent))
          (cl-letf (((symbol-function 'process-send-string)
                     (lambda (process _string)
                       ;; Timers run while the send waits for room.
                       (rexel-test--expire (car rexel--writes))
                       (setq size (rexel--write-size (car rexel--writes))
                             live (process-live-p process))
                       (error "Output file descriptor is closed")))
                    ((symbol-function 'message)
                     (lambda (format &rest args)
                       (push (apply #'format format args) messages))))
            (rexel-test--upload-welcome (car opened)))
          (should-not live)
          (should (= 4096 size))
          (should-not rexel--uploads)
          (let ((failures (seq-filter (lambda (m) (string-match-p "upload failed" m))
                                      messages)))
            (should (= 1 (length failures)))
            (should (string-match-p "connection stalled" (car failures))))
          (should-not (seq-find (lambda (timer)
                                  (memq (timer--function timer)
                                        '(rexel--write-stalled rexel--upload-send-chunk)))
                                timer-list)))
      (delete-file file))))

(ert-deftest rexel-test-upload-sends-compressed-file-as-is ()
  ;; Emacs compresses the text on writing it to a .gz file.
  (let* ((text (make-string 1000 ?x))
         (file (make-temp-file "rexel-upload" nil ".gz" text))
         (size (file-attribute-size (file-attributes file))))
    (unwind-protect
        (rexel-test--with-uploads
          (should (< size (length text)))
          (rexel--send-files (list file))
          (rexel-test--run-upload-timers)
          (should (equal file (rexel--upload-file (car rexel--uploads))))
          (should (= size (rexel--upload-size (car rexel--uploads))))
          (should-not (rexel--upload-temp (car rexel--uploads))))
      (delete-file file))))

(ert-deftest rexel-test-upload-sends-files-in-order ()
  "A file is sent once the one before it closed with status 1000."
  (let ((a (rexel-test--upload-file 10))
        (b (rexel-test--upload-file 20)))
    (unwind-protect
        (rexel-test--with-uploads
          (rexel--send-files (list a b))
          (rexel-test--run-upload-timers)
          (dolist (request (reverse sent))
            (rexel-test--upload-ticket rexel--conn request))
          (pcase-let ((`(,second ,first) opened))
            (rexel-test--upload-welcome second)
            (rexel-test--run-upload-timers)
            (should (= 1 (length frames)))
            (rexel-test--upload-welcome first)
            (rexel-test--run-upload-timers)
            (should (equal '(10 0)
                           (mapcar #'length
                                   (cdr (rexel-test--upload-payloads
                                         frames (car first))))))
            (should (= 1 (length (rexel-test--upload-payloads
                                  frames (car second)))))
            (rexel-test--upload-close first 1000)
            (rexel-test--run-upload-timers)
            (should (equal '(20 0)
                           (mapcar #'length
                                   (cdr (rexel-test--upload-payloads
                                         frames (car second))))))
            (should rexel--uploads)
            (rexel-test--upload-close second 1000)
            (should-not rexel--uploads)))
      (delete-file a)
      (delete-file b))))

(ert-deftest rexel-test-upload-failure-closes-batch-and-empties-queue ()
  (let ((file (rexel-test--upload-file 10)))
    (unwind-protect
        (dolist (failure
                 (list
                  ;; REJECT of the second HELLO.
                  (lambda (_requests opened)
                    (pcase-let ((`(,ws ,on-open ,on-message ,_) (car opened)))
                      (funcall on-open ws)
                      (funcall on-message ws
                               (make-websocket-frame
                                :opcode 'binary :completep t
                                :payload (unibyte-string #x82 1)))))
                  ;; The server breaks off the file being sent.
                  (lambda (_requests opened)
                    (rexel-test--upload-welcome (cadr opened))
                    (rexel-test--upload-close (cadr opened) 4006))
                  ;; A connection drops while its file waits for its turn.
                  (lambda (_requests opened)
                    (rexel-test--upload-close (car opened) nil))
                  ;; The connection ends in a frame too long to read.
                  (lambda (_requests opened)
                    (pcase-let ((`(,ws ,_ ,_ ,on-close) (car opened)))
                      (process-put (websocket-conn ws) :upload-in
                                   (unibyte-string #x82 127 0 0 0 1 0 0 0 0))
                      (funcall on-close ws)))
                  ;; A third file is refused.
                  (lambda (requests _opened)
                    (rexel-test--respond
                     rexel--conn (car requests) nil
                     '((code . "too_large") (message . "too many uploads"))))))
          (rexel-test--with-uploads
            (rexel--send-files (list file file file))
            (rexel-test--run-upload-timers)
            ;; Dropped while the batch runs, these wait in the queue.
            (rexel--send-files (list file))
            (rexel-test--run-upload-timers)
            (should (= 3 (length sent)))
            (should (equal (list file) rexel--upload-queue))
            (rexel-test--upload-ticket rexel--conn (nth 2 sent))
            (rexel-test--upload-ticket rexel--conn (nth 1 sent))
            (should (= 2 (length opened)))
            (funcall failure sent opened)
            (should-not rexel--uploads)
            (should-not rexel--upload-queue)
            (dolist (open opened)
              (should-not (process-live-p (websocket-conn (car open)))))
            (rexel-test--run-upload-timers)
            (should (= 3 (length sent)))))
      (delete-file file))))

(ert-deftest rexel-test-upload-fails-on-changed-file ()
  (let ((file (rexel-test--upload-file 100)))
    (unwind-protect
        (rexel-test--with-uploads
          (rexel--send-files (list file))
          (rexel-test--run-upload-timers)
          (write-region "short" nil file nil 'silent)
          (rexel-test--upload-ticket rexel--conn (car sent))
          (rexel-test--upload-welcome (car opened))
          (should-not rexel--uploads)
          (should-not (process-live-p (websocket-conn (caar opened)))))
      (delete-file file))))

(ert-deftest rexel-test-upload-follows-symlink ()
  "A symlink is sent with the size of the file it points to."
  (let* ((file (rexel-test--upload-file 5000))
         (link (concat file "-link")))
    (unwind-protect
        (rexel-test--with-uploads
          (make-symbolic-link file link)
          (rexel--send-files (list link))
          (rexel-test--run-upload-timers)
          (should (= 5000 (alist-get 'size (rexel-test--upload-args (car sent))))))
      (delete-file link)
      (delete-file file))))

(ert-deftest rexel-test-upload-overdue-write-holds-chunks-back ()
  "An overdue write gets to return, and then chunks go on."
  (let ((file (rexel-test--upload-file 10))
        (writes 0)
        overdue held
        (rexel--writes-overdue nil))
    (unwind-protect
        (rexel-test--with-uploads
          (rexel--send-files (list file file))
          (rexel-test--run-upload-timers)
          (rexel-test--upload-ticket rexel--conn (nth 1 sent))
          (rexel-test--upload-ticket rexel--conn (nth 0 sent))
          (let ((buf (current-buffer))
                (first (car rexel--uploads))
                (second (cadr rexel--uploads)))
            (funcall (nth 1 (car opened)) (caar opened))
            (funcall (nth 2 (car opened)) (caar opened)
                     (make-websocket-frame
                      :opcode 'binary :completep t
                      :payload (concat (unibyte-string #x81) (make-string 31 0))))
            (setf (rexel--upload-state second) 'sending)
            (cl-letf (((symbol-function 'process-send-string)
                       (lambda (_process _string)
                         (when (= 1 (cl-incf writes))
                           ;; A write of another block runs in this wait.
                           (let* ((outer (car rexel--writes))
                                  (rexel--writes
                                   (cons (rexel--make-write :process 'other)
                                         rexel--writes)))
                             (rexel-test--expire outer))
                           (setq overdue rexel--writes-overdue)
                           (rexel--upload-send-chunk buf second)
                           (setq held writes)))))
              (rexel-test--upload-welcome (cadr opened)))
            (should overdue)
            (should (= 1 held))
            (should-not rexel--writes-overdue)
            (should (seq-find (lambda (timer)
                                (and (eq (timer--function timer)
                                         #'rexel--upload-send-chunk)
                                     (eq (nth 1 (timer--args timer)) second)))
                              timer-list))
            (rexel--upload-fail buf "done")))
      (delete-file file))))

(ert-deftest rexel-test-upload-stall-of-ended-batch-fails-nothing ()
  "The timer of a batch that ended leaves the next batch alone."
  (let ((file (rexel-test--upload-file 10)))
    (unwind-protect
        (rexel-test--with-uploads
          (rexel--send-files (list file))
          (rexel-test--run-upload-timers)
          (rexel-test--upload-ticket rexel--conn (car sent))
          (cl-letf (((symbol-function 'process-send-string)
                     (lambda (_process _string)
                       (rexel--upload-fail (current-buffer) "cancelled")
                       (setq rexel--uploads (list (rexel--make-upload)))
                       (rexel-test--expire (car rexel--writes)))))
            (rexel-test--upload-welcome (car opened)))
          (should rexel--uploads))
      (delete-file file))))

(ert-deftest rexel-test-upload-writes-nest ()
  "A chunk due while another write waits for room is sent inside that wait."
  (let ((file (rexel-test--upload-file 10))
        (writes 0)
        second nested)
    (unwind-protect
        (rexel-test--with-uploads
          (let ((buf (current-buffer)))
            (rexel--send-files (list file file))
            (rexel-test--run-upload-timers)
            (rexel-test--upload-ticket rexel--conn (nth 1 sent))
            (rexel-test--upload-ticket rexel--conn (nth 0 sent))
            (setq second (cadr rexel--uploads))
            (funcall (nth 1 (car opened)) (caar opened))
            (funcall (nth 2 (car opened)) (caar opened)
                     (make-websocket-frame
                      :opcode 'binary :completep t
                      :payload (concat (unibyte-string #x81) (make-string 31 0))))
            (setf (rexel--upload-state second) 'sending)
            (cl-letf (((symbol-function 'process-send-string)
                       (lambda (_process _string)
                         (when (= 1 (cl-incf writes))
                           (rexel--upload-send-chunk buf second)
                           (setq nested (list writes (length rexel--writes)))))))
              (rexel-test--upload-welcome (cadr opened)))
            (should (equal '(2 1) nested))
            (should (= 2 writes))
            (should-not rexel--writes)
            (rexel--upload-fail buf "done")))
      (delete-file file))))

(ert-deftest rexel-test-upload-next-waits-for-writes ()
  "No batch starts with a write innermost, as a fetch could block it."
  (let ((file (rexel-test--upload-file 10)))
    (unwind-protect
        (rexel-test--with-uploads
          (setq rexel--upload-queue (list file))
          (let ((rexel--writes (list (rexel--make-write))))
            (rexel--upload-next (current-buffer)))
          (should-not rexel--uploads)
          (should-not sent)
          (rexel-test--run-upload-timers)
          (should rexel--uploads)
          (should sent))
      (delete-file file))))

(ert-deftest rexel-test-upload-next-runs-beside-a-handler ()
  "A prompt in a message handler does not hold a batch back."
  (let ((file (rexel-test--upload-file 10)))
    (unwind-protect
        (rexel-test--with-uploads
          (setq rexel--upload-queue (list file))
          (let ((rexel--writes (list 'handler (rexel--make-write))))
            (rexel--upload-next (current-buffer)))
          (should rexel--uploads))
      (delete-file file))))

(ert-deftest rexel-test-upload-start-failures ()
  "A batch that cannot start is given up and reported."
  (let ((file (rexel-test--upload-file 10))
        messages)
    (unwind-protect
        (cl-letf (((symbol-function 'message)
                   (lambda (format &rest args)
                     (push (apply #'format format args) messages))))
          (dolist (case
                   `(("Not connected"
                      rexel--ready-p ,(lambda (_conn) nil))
                     ("no such host"
                      rexel--local-copy ,(lambda (_file) (error "no such host")))
                     ("Cannot read"
                      file-truename ,(lambda (_file) "/nonexistent/rexel-test"))))
            (rexel-test--with-uploads
              (setq rexel--upload-queue (list file))
              (cl-letf (((symbol-function (nth 1 case)) (nth 2 case)))
                (rexel--upload-next (current-buffer)))
              (should-not rexel--uploads)
              (should-not sent)
              (should (string-match-p (car case) (car messages))))))
      (delete-file file))))

(ert-deftest rexel-test-upload-fetch-error-spares-a-newer-batch ()
  "A fetch failing after its batch ended fails no other batch."
  (let ((file (rexel-test--upload-file 10))
        (newer (rexel--make-upload :name "newer"))
        (fetches 0))
    (unwind-protect
        (rexel-test--with-uploads
          (setq rexel--upload-queue (list file file))
          (cl-letf (((symbol-function 'rexel--local-copy)
                     (lambda (_file)
                       (cl-incf fetches)
                       (setq rexel--uploads (list newer))
                       (error "Lost"))))
            (rexel--upload-next (current-buffer)))
          (should (equal (list newer) rexel--uploads))
          (should (= 1 fetches)))
      (delete-file file))))

(ert-deftest rexel-test-upload-fetch-stops-with-its-batch ()
  (let ((file (rexel-test--upload-file 10))
        (fetches 0))
    (unwind-protect
        (rexel-test--with-uploads
          (setq rexel--upload-queue (list file file))
          (cl-letf (((symbol-function 'rexel--local-copy)
                     (lambda (_file)
                       (cl-incf fetches)
                       (rexel--upload-fail (current-buffer) "cancelled")
                       nil)))
            (rexel--upload-next (current-buffer)))
          (should (= 1 fetches))
          (should-not sent))
      (delete-file file))))

(ert-deftest rexel-test-upload-send-error-leaves-no-stall-timer ()
  (let ((file (rexel-test--upload-file 10)))
    (unwind-protect
        (rexel-test--with-uploads
          (rexel--send-files (list file))
          (rexel-test--run-upload-timers)
          (rexel-test--upload-ticket rexel--conn (car sent))
          (cl-letf (((symbol-function 'process-send-string)
                     (lambda (&rest _) (error "Broken pipe"))))
            (rexel-test--upload-welcome (car opened)))
          (should-not rexel--uploads)
          (should-not (seq-find (lambda (timer)
                                  (eq (timer--function timer) #'rexel--write-stalled))
                                timer-list)))
      (delete-file file))))

(ert-deftest rexel-test-upload-fails-when-its-write-is-interrupted ()
  "A throw through a write fails the batch."
  (let ((file (rexel-test--upload-file 10))
        messages)
    (unwind-protect
        (rexel-test--with-uploads
          (rexel--send-files (list file))
          (rexel-test--run-upload-timers)
          (rexel-test--upload-ticket rexel--conn (car sent))
          (cl-letf (((symbol-function 'process-send-string)
                     (lambda (&rest _) (throw 'interrupted nil)))
                    ((symbol-function 'message)
                     (lambda (format &rest args)
                       (push (apply #'format format args) messages))))
            (catch 'interrupted
              (rexel-test--upload-welcome (car opened))))
          (should-not rexel--uploads)
          (should-not (process-live-p (websocket-conn (caar opened))))
          (should (string-match-p "interrupted" (car messages)))
          (should (= 1 (length messages))))
      (delete-file file))))

(ert-deftest rexel-test-upload-stalled-then-interrupted-reports-once ()
  (let ((file (rexel-test--upload-file 10))
        messages)
    (unwind-protect
        (rexel-test--with-uploads
          (rexel--send-files (list file))
          (rexel-test--run-upload-timers)
          (rexel-test--upload-ticket rexel--conn (car sent))
          (cl-letf (((symbol-function 'process-send-string)
                     (lambda (&rest _)
                       (rexel-test--expire (car rexel--writes))
                       (throw 'interrupted nil)))
                    ((symbol-function 'message)
                     (lambda (format &rest args)
                       (push (apply #'format format args) messages))))
            (catch 'interrupted
              (rexel-test--upload-welcome (car opened))))
          (should (= 1 (length messages)))
          (should (string-match-p "connection stalled" (car messages))))
      (delete-file file))))

(ert-deftest rexel-test-upload-reports-unreadable-file ()
  "A file that stops being readable reports the reason."
  (let ((file (rexel-test--upload-file 10))
        messages)
    (unwind-protect
        (rexel-test--with-uploads
          (rexel--send-files (list file))
          (rexel-test--run-upload-timers)
          (rexel-test--upload-ticket rexel--conn (car sent))
          (delete-file file)
          (cl-letf (((symbol-function 'message)
                     (lambda (format &rest args)
                       (push (apply #'format format args) messages))))
            (rexel-test--upload-welcome (car opened)))
          (should-not rexel--uploads)
          (should (string-match-p "No such file" (car messages))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest rexel-test-upload-close-before-end-fails ()
  "A clean close before the end marker went out is no success."
  (let ((file (rexel-test--upload-file 10))
        messages)
    (unwind-protect
        (rexel-test--with-uploads
          (rexel--send-files (list file))
          (rexel-test--run-upload-timers)
          (rexel-test--upload-ticket rexel--conn (car sent))
          (rexel-test--upload-welcome (car opened))
          (should (eq 'sending (rexel--upload-state (car rexel--uploads))))
          (cl-letf (((symbol-function 'message)
                     (lambda (format &rest args)
                       (push (apply #'format format args) messages))))
            (rexel-test--upload-close (car opened) 1000))
          (should-not rexel--uploads)
          (should (string-match-p "upload failed" (car messages))))
      (delete-file file))))

(ert-deftest rexel-test-upload-ignores-events-of-a-failed-batch ()
  "What arrives for a batch that failed changes nothing."
  (let ((file (rexel-test--upload-file 10))
        messages)
    (unwind-protect
        (rexel-test--with-uploads
          (rexel--send-files (list file file))
          (rexel-test--run-upload-timers)
          (rexel-test--upload-ticket rexel--conn (nth 1 sent))
          (let ((upload (car rexel--uploads))
                (open (car opened)))
            (funcall (nth 1 open) (car open))
            (rexel--upload-fail (current-buffer) "cancelled")
            (cl-letf (((symbol-function 'message)
                       (lambda (format &rest args)
                         (push (apply #'format format args) messages))))
              ;; A late ticket opens no connection.
              (rexel-test--upload-ticket rexel--conn (nth 0 sent))
              (should (= 1 (length opened)))
              (funcall (nth 2 open) (car open)
                       (make-websocket-frame
                        :opcode 'binary :completep t
                        :payload (concat (unibyte-string #x81) (make-string 31 0))))
              (should (eq 'welcome (rexel--upload-state upload)))
              (funcall (nth 3 open) (car open))
              (rexel--upload-send-chunk (current-buffer) upload))
            (should-not messages)
            (should (= 0 (rexel--upload-sent upload)))
            (should-not rexel--uploads)))
      (delete-file file))))

(ert-deftest rexel-test-upload-deletes-copy-on-failure ()
  (let ((file (rexel-test--upload-file 10))
        (copy (rexel-test--upload-file 10)))
    (unwind-protect
        (rexel-test--with-uploads
          (cl-letf (((symbol-function 'rexel--local-copy)
                     (lambda (_file) copy)))
            (rexel--send-files (list file))
            (rexel-test--run-upload-timers))
          (should (file-exists-p copy))
          (rexel--upload-fail (current-buffer) "cancelled")
          (should-not (file-exists-p copy)))
      (delete-file file)
      (when (file-exists-p copy)
        (delete-file copy)))))

(ert-deftest rexel-test-upload-fetch-sees-buffer-killed ()
  "A block closed while a remote file is fetched leaves nothing behind."
  (let ((file (rexel-test--upload-file 10))
        (copy (rexel-test--upload-file 10))
        (block (generate-new-buffer " *rexel-test-block*"))
        (elsewhere (generate-new-buffer " *rexel-test-other*")))
    (unwind-protect
        (rexel-test--with-uploads
          (let ((conn rexel--conn))
            (with-current-buffer block
              (setq rexel--conn conn rexel--session-id "s" rexel--block-id "b"
                    rexel--upload-queue (list file))
              (add-hook 'kill-buffer-hook
                        (lambda () (rexel--upload-fail (current-buffer) "gone"))
                        nil t))
            (with-current-buffer elsewhere
              (cl-letf (((symbol-function 'rexel--local-copy)
                         (lambda (_file) (kill-buffer block) copy)))
                (rexel--upload-next block))
              (should-not rexel--uploads))
            (should-not sent)
            (should-not (file-exists-p copy))))
      (delete-file file)
      (kill-buffer elsewhere)
      (when (buffer-live-p block)
        (kill-buffer block))
      (when (file-exists-p copy)
        (delete-file copy)))))

(ert-deftest rexel-test-upload-fetch-starts-no-second-batch ()
  "A timer running during a fetch does not start another batch."
  (let ((file (rexel-test--upload-file 10))
        fetches)
    (unwind-protect
        (rexel-test--with-uploads
          (let ((buf (current-buffer)))
            (setq rexel--upload-queue (make-list 21 file))
            (cl-letf (((symbol-function 'rexel--local-copy)
                       (lambda (_file)
                         (when (= 1 (cl-incf fetches 1))
                           (rexel--upload-next buf))
                         nil)))
              (setq fetches 0)
              (rexel--upload-next buf))
            (should (= 16 (length rexel--uploads)))
            (should (= 16 (length sent)))
            (should (= 5 (length rexel--upload-queue)))
            (should (= 1 (length (seq-uniq
                                  (mapcar (lambda (object)
                                            (alist-get 'batch (rexel-test--upload-args object)))
                                          sent)))))))
      (delete-file file))))

(ert-deftest rexel-test-upload-batches ()
  "Files of one command share a batch; a batch holds 16 files."
  (let ((file (rexel-test--upload-file 10)))
    (unwind-protect
        (progn
          (rexel-test--with-uploads
            (dotimes (_ 3)
              (rexel--send-files (list file)))
            (rexel-test--run-upload-timers)
            (should (equal '(3 3 3)
                           (mapcar (lambda (request)
                                     (alist-get 'count
                                                (rexel-test--upload-args request)))
                                   sent))))
          (rexel-test--with-uploads
            (rexel--send-files (make-list 17 file))
            (rexel-test--run-upload-timers)
            (should (= 16 (length sent)))
            (should (= 16 (alist-get 'count (rexel-test--upload-args (car sent)))))
            (dolist (request (reverse sent))
              (rexel-test--upload-ticket rexel--conn request))
            (dolist (open (reverse opened))
              (rexel-test--upload-welcome open))
            (dolist (open (reverse opened))
              (rexel-test--run-upload-timers)
              (rexel-test--upload-close open 1000))
            ;; The rest follows as a batch of its own, outside the sentinel
            ;; that ended this one: a remote file can take long to fetch.
            (should (= 16 (length sent)))
            (rexel-test--run-upload-timers)
            (should (= 17 (length sent)))
            (should (= 1 (length rexel--uploads)))
            (should-not (assq 'batch (rexel-test--upload-args (car sent))))))
      (delete-file file))))

(ert-deftest rexel-test-upload-file-sends-to-block-buffer ()
  (let ((file (rexel-test--upload-file 3)))
    (unwind-protect
        (rexel-test--with-uploads
          (let ((block (current-buffer))
                (default-directory (file-name-directory file)))
            (with-temp-buffer
              (rexel-upload-file (list (file-name-nondirectory file)) block)))
          (should (equal (list file) rexel--upload-queue))
          (rexel-test--run-upload-timers)
          (should (= 1 (length sent)))
          (rexel-cancel-upload)
          (should-error (rexel-cancel-upload) :type 'user-error))
      (delete-file file))))

(ert-deftest rexel-test-control-close-fails-uploads ()
  (let ((file (rexel-test--upload-file 10)))
    (unwind-protect
        (rexel-test--with-uploads
          (rexel--send-files (list file))
          (rexel-test--run-upload-timers)
          (should rexel--uploads)
          (let ((rexel--conns (list rexel--conn)))
            (rexel--on-control-close rexel--conn))
          (should-not rexel--uploads))
      (delete-file file))))

(ert-deftest rexel-test-ws-open-gives-up-on-silent-unix-socket ()
  "A Unix socket that accepts and never answers counts as closed."
  (let* ((socket (make-temp-name
                  (expand-file-name "rexel-test" temporary-file-directory)))
         (server (make-network-process :name "rexel-test-server" :server t
                                       :family 'local :service socket :noquery t))
         (rexel--connect-timeout 0.05)
         closed)
    (unwind-protect
        (let ((ws (rexel--ws-open socket "/api/data" "p" #'ignore #'ignore
                                  (lambda (_ws) (setq closed t)))))
          (with-timeout (3 nil)
            (while (not closed)
              (accept-process-output nil 0.01)))
          (should closed)
          (should-not (process-live-p (websocket-conn ws))))
      (delete-process server)
      (when (file-exists-p socket)
        (delete-file socket)))))

(ert-deftest rexel-test-ws-open-keeps-an-opened-connection ()
  "The open timeout leaves a connection alone once it opened, or its answer
is waiting to be read."
  (let* ((socket (make-temp-name
                  (expand-file-name "rexel-test" temporary-file-directory)))
         (server (make-network-process :name "rexel-test-server" :server t
                                       :family 'local :service socket :noquery t))
         (rexel--connect-timeout 0.02))
    (unwind-protect
        (let ((opened (rexel--ws-open socket "/api/data" "p"
                                      #'ignore #'ignore #'ignore))
              (waiting (rexel--ws-open socket "/api/data" "p"
                                       #'ignore #'ignore #'ignore)))
          (setf (websocket-ready-state opened) 'open)
          (cl-letf (((symbol-function 'accept-process-output)
                     (lambda (&rest _)
                       (setf (websocket-ready-state waiting) 'open))))
            (sleep-for 0.1))
          (should (process-live-p (websocket-conn opened)))
          (should (process-live-p (websocket-conn waiting)))
          (delete-process (websocket-conn opened))
          (delete-process (websocket-conn waiting)))
      (delete-process server)
      (when (file-exists-p socket)
        (delete-file socket)))))

(ert-deftest rexel-test-binary-frame ()
  "A frame is final, binary and masked with a zero key."
  (should (equal (unibyte-string #x82 #x80 0 0 0 0) (rexel--binary-frame "")))
  (should (equal (concat (unibyte-string #x82 #xfd 0 0 0 0) (make-string 125 ?x))
                 (rexel--binary-frame (make-string 125 ?x))))
  (should (equal (concat (unibyte-string #x82 #xfe 0 126 0 0 0 0)
                         (make-string 126 ?x))
                 (rexel--binary-frame (make-string 126 ?x))))
  (let* ((payload (apply #'unibyte-string (mapcar (lambda (i) (% i 256))
                                                  (number-sequence 0 4095))))
         (bytes (rexel--binary-frame payload))
         (frame (websocket-read-frame bytes)))
    (should-not (multibyte-string-p bytes))
    (should (equal (unibyte-string #x82 #xfe 16 0 0 0 0 0) (substring bytes 0 8)))
    (should (eq 'binary (websocket-frame-opcode frame)))
    (should (websocket-frame-completep frame))
    (should (equal payload (websocket-frame-payload frame)))))

(ert-deftest rexel-test-close-status ()
  (let ((welcome (concat (unibyte-string #x82 32 #x81) (make-string 31 0)))
        (ping (concat (unibyte-string #x89 5) "hello"))
        (long (concat (unibyte-string #x82 126 1 44) (make-string 300 ?x)))
        (close (concat (unibyte-string #x88 26 #x0f #xa6)
                       "data stream broke a rule")))
    (should (= 4006 (rexel--close-status (concat welcome ping long ping close))))
    (should (= 1000 (rexel--close-status
                     (concat "HTTP/1.1 101 Switching Protocols\r\n"
                             "Upgrade: websocket\r\n\r\n"
                             welcome (unibyte-string #x88 2 3 #xe8)))))
    (should (= 1005 (rexel--close-status (unibyte-string #x88 0))))
    (should-not (rexel--close-status (concat welcome ping)))
    (should-not (rexel--close-status ""))
    ;; Cut off inside the close frame, its length, and the handshake.
    (should-not (rexel--close-status (concat welcome (substring close 0 3))))
    (should-not (rexel--close-status (concat welcome (unibyte-string #x88))))
    (should-not (rexel--close-status (concat ping (substring long 0 2))))
    (should-not (rexel--close-status "HTTP/1.1 101 Switching"))
    ;; A length websocket.el refuses to read.
    (should-not (rexel--close-status (unibyte-string #x82 127 0 0 0 1 0 0 0 0)))))

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
                bash))
    ;; The wrapper is reported before the shell it starts, also before
    ;; it execs `login'.
    (should-not (rexel--shell-process login nil))
    (should-not (rexel--shell-process '((name . "rex") (pid . 1)) nil))
    ;; A `rex' run from the shell is a command like any other.
    (should (equal bash (rexel--shell-process '((name . "rex") (pid . 3))
                                              (vector bash login))))))

(defun rexel-test--process-event (fg &rest ancestors)
  "Return the event that FG runs in block \"b\" below ANCESTORS."
  `((block_id . "b") (creator_name . ,rexel--terminal-creator)
    (name . "process_changed")
    (payload . ((foreground . ,fg) (ancestors . ,(vconcat ancestors))))))

(ert-deftest rexel-test-command-running-follows-processes ()
  "A command runs while another process than the shell is in the foreground."
  (rexel-test--with-fake-conn _sent
    (puthash "b" (current-buffer) (rexel--conn-blocks rexel--conn))
    (let ((login '((name . "login") (pid . 1)))
          (bash '((name . "bash") (pid . 2)))
          (sleep '((name . "sleep") (pid . 3))))
      (rexel--on-block-event rexel--conn (rexel-test--process-event login))
      (should-not rexel--shell-pid)
      (rexel--on-block-event rexel--conn (rexel-test--process-event bash login))
      (should (= 2 rexel--shell-pid))
      (should-not ghostel--command-running)
      (rexel--on-block-event rexel--conn
                             (rexel-test--process-event sleep bash login))
      (should ghostel--command-running)
      ;; Without ancestors the shell stays known.
      (rexel--on-block-event rexel--conn (rexel-test--process-event sleep))
      (should (= 2 rexel--shell-pid))
      (rexel--on-block-event rexel--conn (rexel-test--process-event bash login))
      (should-not ghostel--command-running))))

(ert-deftest rexel-test-exec-ends-prompt-marks ()
  "OSC 133 marks hide the reported processes until the shell execs."
  (rexel-test--with-fake-conn _sent
    (puthash "b" (current-buffer) (rexel--conn-blocks rexel--conn))
    (let* ((login '((name . "login") (pid . 1)))
           (shell '((name . "bash") (pid . 2) (path . "/opt/bash") (argv0 . "-bash")))
           (plain '((name . "bash") (pid . 2) (path . "/bin/bash") (argv0 . "bash")))
           (sleep '((name . "sleep") (pid . 3)))
           (report (lambda (&rest processes)
                     (rexel--on-block-event
                      rexel--conn (apply #'rexel-test--process-event processes)))))
      (funcall report shell login)
      (ghostel--osc133-marker "A" nil)
      ;; The shell's own marks say when a command runs.
      (funcall report sleep shell login)
      (should-not ghostel--command-running)
      ;; The command is `exec'; no mark follows it.
      (ghostel--osc133-marker "C" nil)
      (funcall report shell login)
      (should ghostel--command-running)
      (funcall report sleep plain login)
      (should ghostel--command-running)
      (funcall report plain login)
      (should-not ghostel--command-running)
      ;; A shell that marks its prompt takes over again.
      (ghostel--osc133-marker "A" nil)
      (funcall report sleep plain login)
      (should-not ghostel--command-running))))

(ert-deftest rexel-test-exec-noticed-without-ancestors ()
  "An exec shows in a report of the shell alone, under the same pid."
  (rexel-test--with-fake-conn _sent
    (puthash "b" (current-buffer) (rexel--conn-blocks rexel--conn))
    (let ((shell '((name . "bash") (pid . 2) (path . "/opt/bash") (argv0 . "-bash")))
          (plain '((name . "bash") (pid . 2) (path . "/bin/bash") (argv0 . "bash")))
          (other '((name . "zsh") (pid . 5) (path . "/bin/zsh") (argv0 . "zsh")))
          (sleep '((name . "sleep") (pid . 3))))
      (rexel--on-block-event rexel--conn (rexel-test--process-event shell))
      (ghostel--osc133-marker "A" nil)
      (rexel--on-block-event rexel--conn (rexel-test--process-event plain))
      (rexel--on-block-event rexel--conn (rexel-test--process-event sleep plain))
      (should ghostel--command-running)
      ;; Another process as the shell is no exec.
      (rexel--on-block-event rexel--conn (rexel-test--process-event plain))
      (ghostel--osc133-marker "A" nil)
      (rexel--on-block-event rexel--conn (rexel-test--process-event sleep other))
      (should (= 5 rexel--shell-pid))
      (should-not ghostel--command-running))))

(ert-deftest rexel-test-exec-survives-stream-restart ()
  "Prompt marks an exec ended stay ended when the block is streamed afresh."
  (rexel-test--with-fake-conn _sent
    (let ((conn rexel--conn)
          (mark '(fake)))
      (setq ghostel--prompt-positions (list mark)
            rexel--stale-prompt mark)
      (cl-letf (((symbol-function 'rexel-block-mode) #'ignore))
        (rexel--attach-block-buffer conn "s" "b" (current-buffer)))
      (should (eq mark rexel--stale-prompt)))))

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
