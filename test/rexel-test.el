;;; rexel-test.el --- Tests for rexel -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests of the stream parsing that needs no server: the snapshot
;; boundary walk and the splitting of terminal output from Rex's
;; nonce-tagged control messages.

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
                 (lambda (_path _url _protocol on-open &rest _)
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
  (with-temp-buffer
    (setq rexel--data-state 'welcome rexel--stream (rexel--make-stream))
    (rexel--on-welcome (unibyte-string #x82 3))
    (should (eq rexel--data-state 'closed))
    (should-not rexel--stream)))

(ert-deftest rexel-test-control-welcome-checks-revision ()
  (pcase-dolist (`(,protocol ,revision ,ok)
                 '(("rex.control.v1" 8 t) ("rex.control.v1" 7 nil)
                   ("rex.control.v1" nil nil) ("other" 8 nil)))
    (let ((conn (rexel--make-conn :socket "none"))
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
  (let* ((rexel-hosts '(("studio" . "/tmp/studio.sock")))
         (local (make-temp-file "rexel"))
         (endpoint (concat "unix://" (url-hexify-string local))))
    (unwind-protect
        (cl-letf (((symbol-function 'rexel--rex-json)
                   (lambda (&rest args)
                     (if (equal (car args) "server")
                         `((endpoint . ,endpoint))
                       '((hosts . [((label . "studio") (endpoints . ["unix:///x.sock"]))
                                   ((label . "far") (endpoints . ["far.ts.net"]))
                                   ((label . "near")
                                    (endpoints . ["near.ts.net" "unix:///tmp/near.sock"]))]))))))
          (should (equal `((nil . ,local)
                           ("studio" . "/tmp/studio.sock")
                           ("near" . "/tmp/near.sock"))
                         (rexel--hosts))))
      (delete-file local))))

(ert-deftest rexel-test-buffer-name-carries-host ()
  (let ((conn (rexel--make-conn :socket "none" :host "studio")))
    (setf (alist-get "s" (rexel--conn-sessions conn) nil nil #'equal) "work")
    (should (equal "*rex:work@studio:123456*"
                   (rexel--buffer-name conn "s" "block:123456")))))

(ert-deftest rexel-test-apply-view-renames-on-label-change ()
  (let* ((conn (rexel--make-conn :socket "none"))
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
  `(let ((rexel--conn (rexel--make-conn :socket "none"))
         (,sent nil))
     (cl-letf (((symbol-function 'rexel--send)
                (lambda (_conn object) (push object ,sent))))
       (with-temp-buffer
         (setq rexel--session-id "s" rexel--block-id "b")
         ,@body))))

(defun rexel-test--sent-data (object)
  "Return the typed text in the write request OBJECT."
  (alist-get 'data (alist-get 'args (alist-get 'payload object))))

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
          (should-not (websocket-inflight-input ws)))
      (delete-process proc))))

(provide 'rexel-test)
;;; rexel-test.el ends here
