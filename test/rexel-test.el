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

(ert-deftest rexel-test-snapshot-end ()
  (should (= (length rexel-test--snapshot)
             (rexel--snapshot-end rexel-test--snapshot)))
  (should (= (length rexel-test--snapshot)
             (rexel--snapshot-end (concat rexel-test--snapshot "live bytes"))))
  (should-not (rexel--snapshot-end (substring rexel-test--snapshot 0 100)))
  ;; FINISH header seen but a byte short of complete.
  (should-not (rexel--snapshot-end
               (substring rexel-test--snapshot 0 -1)))
  (should-not (rexel--snapshot-end "GHOST"))
  (should-not (rexel--snapshot-end "not a snapshot at all")))

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

(ert-deftest rexel-test-u64le ()
  (should (= 1 (rexel--u64le (unibyte-string 1 0 0 0 0 0 0 0) 0)))
  (should (= (ash 1 56) (rexel--u64le (unibyte-string 9 0 0 0 0 0 0 0 1) 1))))

(ert-deftest rexel-test-welcome-reads-nonce ()
  (with-temp-buffer
    (setq rexel--data-state 'welcome)
    (rexel--on-welcome (concat (unibyte-string #x81) (make-string 16 ?t)
                               (unibyte-string 0 0 0 1 0 0 0)
                               (unibyte-string 2 1 0 0 0 0 0 0)))
    (should (eq rexel--data-state 'snapshot))
    (should (equal "\e_rex1;258;" rexel--marker))))

(ert-deftest rexel-test-welcome-reject ()
  (with-temp-buffer
    (setq rexel--data-state 'welcome)
    (rexel--on-welcome (unibyte-string #x82 3))
    (should (eq rexel--data-state 'closed))))

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
    (let ((size '(80 . 24)))
      (setq rexel--data-state 'live)
      (cl-letf (((symbol-function 'rexel--desired-size) (lambda () size)))
        (rexel--sync-size t)
        (should (equal '("session.own_block")
                       (mapcar (lambda (o) (alist-get 'method o)) sent)))
        ;; Sizes requested meanwhile wait; the latest one follows the take.
        (setq size '(100 . 30))
        (rexel--sync-size)
        (setq size '(120 . 40))
        (rexel--sync-size)
        (should (= 1 (length sent)))
        (rexel-test--respond rexel--conn (car sent))
        (should (equal '((columns . 120) (rows . 40))
                       (alist-get 'args (alist-get 'payload (car sent)))))
        ;; An unchanged size sends nothing more.
        (rexel-test--respond rexel--conn (car sent))
        (rexel--sync-size)
        (should (= 2 (length sent)))
        ;; A resize without take sends no own_block.
        (setq size '(90 . 20))
        (rexel--sync-size)
        (should (equal "com.superlogical.terminal.resize"
                       (alist-get 'method (car sent))))
        (should (= 3 (length sent)))))))

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
