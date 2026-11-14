;;; rexel-transfer-test.el --- Tests for rexel-transfer -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests of the uploads and downloads with the connections stubbed and
;; timers run by hand, on the helpers of rexel-test.el.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'rexel)
(require 'rexel-test)

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

(defmacro rexel-test--with-downloads (&rest body)
  "Run BODY as `rexel-test--with-uploads' does, in a temporary directory.
The variable `dir' names it and `said' collects the messages, newest
first."
  (declare (indent 0))
  `(let ((dir (file-name-as-directory (make-temp-file "rexel-download" t)))
         rexel--downloads said)
     (unwind-protect
         (cl-letf (((symbol-function 'message)
                    (lambda (format &rest args)
                      (when format
                        (push (apply #'format format args) said)))))
           (rexel-test--with-uploads ,@body))
       (delete-directory dir t))))

(defun rexel-test--download-begin (conn object size)
  "Answer the `download_begin' request OBJECT on CONN for a file of SIZE bytes."
  (rexel-test--respond
   conn object `((ticket . ,(base64url-encode-string (make-string 16 7) t))
                 (expires_in_ms . 60000)
                 (path . "/srv/log.bin") (name . "log.bin") (size . ,size)
                 (modified_at . "2026-11-14T16:28:56+01:00"))))

(defun rexel-test--download-message (open payload)
  "Hand the binary message PAYLOAD to the data connection OPEN."
  (pcase-let ((`(,ws ,_ ,on-message ,_) open))
    (funcall on-message ws (make-websocket-frame :opcode 'binary :completep t
                                                 :payload payload))))

(defun rexel-test--file-bytes (file)
  "Return the bytes of FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(ert-deftest rexel-test-download-writes-file-at-end-marker ()
  (rexel-test--with-downloads
    (let ((file (expand-file-name "out.bin" dir))
          ;; A part of the file can start like a WELCOME.
          (parts (list (concat (unibyte-string #x81 0 255) (make-string 40 ?a))
                       (unibyte-string #x82 1 10 13 0)))
          got)
      (rexel--download "logs/log.bin" file (lambda (f) (push f got)))
      (should (equal "com.superlogical.terminal.download_begin"
                     (alist-get 'method (car sent))))
      (should (equal '((session_id . "s") (block_id . "b"))
                     (seq-take (alist-get 'payload (car sent)) 2)))
      (let ((args (rexel-test--upload-args (car sent))))
        (should (equal "logs/log.bin" (alist-get 'path args)))
        (should (string-prefix-p "download:" (alist-get 'ref args))))
      (rexel-test--download-begin rexel--conn (car sent) 48)
      (let ((open (car opened)))
        (rexel-test--upload-welcome open)
        (should (equal (list (concat "\1" (make-string 16 7)))
                       (rexel-test--upload-payloads frames (car open))))
        (dolist (part parts)
          (rexel-test--download-message open part))
        (should-not (file-exists-p file))
        (should-not got)
        (rexel-test--download-message open "")
        (should (equal (list file) got))
        (should (equal (apply #'concat parts) (rexel-test--file-bytes file)))
        (should (= (logand #o666 (default-file-modes)) (file-modes file)))
        ;; The server closes the connection after the end marker.
        (funcall (nth 3 open) (car open))
        (should (equal '("out.bin") (directory-files dir nil "[^.]")))
        (should-not (seq-some (lambda (m) (string-search "failed" m)) said))))))

(ert-deftest rexel-test-download-replaces-existing-file ()
  (rexel-test--with-downloads
    (let ((file (expand-file-name "out.bin" dir)))
      (write-region "old content" nil file nil 'quiet)
      (set-file-modes file #o640)
      (rexel--download "log.bin" file)
      (rexel-test--download-begin rexel--conn (car sent) 0)
      (rexel-test--upload-welcome (car opened))
      (should (equal "old content" (rexel-test--file-bytes file)))
      (rexel-test--download-message (car opened) "")
      (should (equal "" (rexel-test--file-bytes file)))
      (should (= #o640 (file-modes file))))))

(ert-deftest rexel-test-download-close-before-end-fails ()
  (rexel-test--with-downloads
    (let ((file (expand-file-name "out.bin" dir))
          got)
      (rexel--download "log.bin" file (lambda (f) (push f got)))
      (rexel-test--download-begin rexel--conn (car sent) 100)
      (let ((open (car opened)))
        (rexel-test--upload-welcome open)
        (rexel-test--download-message open "half")
        (funcall (nth 3 open) (car open))
        (should-not got)
        (should-not (directory-files dir nil "[^.]"))
        (should (equal "rexel: download of log.bin failed: connection lost"
                       (car said)))
        ;; What still arrives changes nothing.
        (rexel-test--download-message open "")
        (should-not (directory-files dir nil "[^.]"))))))

(ert-deftest rexel-test-cancel-download ()
  (rexel-test--with-downloads
    (let ((file (expand-file-name "out.bin" dir)))
      (rexel--download "a.bin" file)
      (rexel-test--download-begin rexel--conn (car sent) 100)
      (rexel-test--upload-welcome (car opened))
      (rexel-test--download-message (car opened) "half")
      ;; One waits for the server's WELCOME, one for its ticket.
      (rexel--download "b.bin" file)
      (rexel-test--download-begin rexel--conn (car sent) 100)
      (rexel--download "c.bin" file)
      (with-temp-buffer
        (should-error (rexel-cancel-download) :type 'user-error))
      (rexel-cancel-download)
      (should (equal '("rexel: download of a.bin failed: cancelled"
                       "rexel: download of b.bin failed: cancelled"
                       "rexel: download of c.bin failed: cancelled")
                     (seq-take said 3)))
      (dolist (open opened)
        (should-not (process-live-p (websocket-conn (car open)))))
      (should-not (directory-files dir nil "[^.]"))
      (should-not rexel--downloads)
      ;; What still arrives changes nothing, and c.bin's ticket is not used.
      (setq said nil)
      (rexel-test--upload-welcome (car opened))
      (rexel-test--download-begin rexel--conn (car sent) 100)
      (should (= 2 (length opened)))
      (should-not said)
      (should-error (rexel-cancel-download) :type 'user-error))))

(ert-deftest rexel-test-download-reports-refusals ()
  (rexel-test--with-downloads
    (let ((file (expand-file-name "out.bin" dir)))
      (rexel--download "gone.txt" file)
      (rexel-test--respond rexel--conn (car sent) nil
                           '((code . "not_found")
                             (message . "not found: /srv/gone.txt does not exist")))
      (should-not opened)
      (should (equal (concat "rexel: download of gone.txt failed: "
                             "not found: /srv/gone.txt does not exist (not_found)")
                     (car said)))
      (rexel--download "log.bin" file)
      (rexel-test--download-begin rexel--conn (car sent) 3)
      (pcase-let ((`(,ws ,on-open ,_ ,_) (car opened)))
        (funcall on-open ws)
        (rexel-test--download-message (car opened) (unibyte-string #x82 1))
        (should-not (process-live-p (websocket-conn ws))))
      (should (equal (concat "rexel: download of log.bin failed: "
                             "data connection rejected, reason 1")
                     (car said)))
      (should-not (directory-files dir nil "[^.]")))))

(ert-deftest rexel-test-control-close-fails-requested-downloads ()
  (rexel-test--with-downloads
    (let ((file (expand-file-name "out.bin" dir))
          (rexel--conns (list rexel--conn)))
      (rexel--download "a.bin" file)
      (rexel-test--download-begin rexel--conn (car sent) 1)
      (rexel-test--upload-welcome (car opened))
      (rexel--download "b.bin" (expand-file-name "b.bin" dir))
      (rexel--on-control-close rexel--conn)
      (should (member "rexel: download of b.bin failed: control connection lost"
                      said))
      ;; A download with its ticket runs on its own connection.
      (rexel-test--download-message (car opened) "x")
      (rexel-test--download-message (car opened) "")
      (should (equal "x" (rexel-test--file-bytes file)))
      (should (equal '("out.bin") (directory-files dir nil "[^.]")))
      (should-not rexel--downloads))))

(ert-deftest rexel-test-download-file-checks-destination ()
  (rexel-test--with-downloads
    (rexel-download-file "logs/log.bin" dir)
    (rexel-test--download-begin rexel--conn (car sent) 0)
    (rexel-test--upload-welcome (car opened))
    (rexel-test--download-message (car opened) "")
    (should (equal '("log.bin") (directory-files dir nil "[^.]")))
    (should-error (rexel-download-file "log.bin" "/ssh:studio:/tmp/log.bin")
                  :type 'user-error)
    (should-error (rexel-download-file
                   "log.bin" (expand-file-name "none/log.bin" dir))
                  :type 'user-error)
    (should (= 1 (length sent)))))

(ert-deftest rexel-test-file-table-completes-in-listed-directories ()
  (rexel-test--with-uploads
    (let* ((cache (make-hash-table :test #'equal))
           (table (rexel--file-table (current-buffer) cache))
           asked)
      (cl-letf (((symbol-function 'process-live-p) #'always)
                ((symbol-function 'accept-process-output)
                 (lambda (&rest _)
                   (let ((args (rexel-test--upload-args (car sent))))
                     (should (eq t (alist-get 'files args)))
                     (push (alist-get 'path args) asked))
                   (rexel-test--respond
                    rexel--conn (car sent)
                    '((path . "/srv/logs") (home . "/home/u") (status . "ok")
                      (entries . [((name . "app.log") (file . t))
                                  ((name . "archive"))
                                  ((name . "app.txt") (file . t))]))))))
        (should (equal "/srv/logs" (car (rexel--list-dir "" cache))))
        (should (eq 'file (completion-metadata-get
                           (completion-metadata "" table nil) 'category)))
        (should (equal '("app.log" "archive/" "app.txt")
                       (all-completions "logs/" table)))
        (should (equal "logs/app." (try-completion "logs/ap" table)))
        (should (equal "logs/archive/" (try-completion "logs/ar" table)))
        (should (test-completion "logs/app.log" table))
        (should (equal '(5 . 1) (completion-boundaries "logs/ap" table nil "p/x")))
        ;; A listing is asked for once.
        (should (equal '("" "logs/") (reverse asked)))
        (all-completions "logs//etc/" table)
        (all-completions "logs/~/" table)
        (should (equal '("~/" "/etc/") (seq-take asked 2)))))))

(ert-deftest rexel-test-file-path-starts-anew ()
  (should (equal "/etc/hosts" (rexel--file-path "/srv/logs//etc/hosts")))
  (should (equal "~/notes" (rexel--file-path "/srv/logs/~/notes")))
  (should (equal "~" (rexel--file-path "/srv/~")))
  (should (equal "/srv/~old/a" (rexel--file-path "/srv/~old/a")))
  (should (equal "logs/app.log" (rexel--file-path "logs/app.log"))))

(ert-deftest rexel-test-download-file-visits ()
  (rexel-test--with-downloads
    (let ((file (expand-file-name "out.txt" dir))
          shown)
      (cl-letf (((symbol-function 'rexel--prompt-later) #'funcall)
                ((symbol-function 'pop-to-buffer)
                 (lambda (buffer &rest _) (push buffer shown))))
        (let ((default-directory dir))
          (rexel-download-file "~/notes/out.txt" "out.txt" t))
        (should (equal "~/notes/out.txt"
                       (alist-get 'path (rexel-test--upload-args (car sent)))))
        (rexel-test--download-begin rexel--conn (car sent) 5)
        (rexel-test--upload-welcome (car opened))
        (rexel-test--download-message (car opened) "hello")
        (should-not shown)
        (rexel-test--download-message (car opened) "")
        (unwind-protect
            (progn
              (should (equal (list (get-file-buffer file)) shown))
              (should (equal "hello" (with-current-buffer (car shown)
                                       (buffer-string)))))
          (mapc #'kill-buffer shown)))
      (with-temp-buffer
        (should-error (rexel-download-file "a.txt" file) :type 'user-error)))))

(ert-deftest rexel-test-upload-waits-for-attach ()
  "A batch starts only once the redialled session is attached."
  (rexel-test--with-fake-conn sent
    (let ((started 0))
      (cl-letf (((symbol-function 'rexel--upload-start)
                 (lambda (_buf) (cl-incf started)))
                ((symbol-function 'run-at-time) #'ignore))
        (setf (rexel--conn-attaching rexel--conn) (list "s"))
        (rexel--upload-next (current-buffer))
        (should (= 0 started))
        (setf (rexel--conn-attaching rexel--conn) nil)
        (rexel--upload-next (current-buffer))
        (should (= 1 started))))))

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

(provide 'rexel-transfer-test)
;;; rexel-transfer-test.el ends here
