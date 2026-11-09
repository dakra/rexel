;;; consult-rexel-test.el --- Tests for consult-rexel -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests of what needs no server and no minibuffer: the candidates, the
;; requests and the preview states, with `rexel--request' stubbed.  The
;; pickers themselves run in `consult--multi' and are tested live.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'consult-rexel)

(defvar embark-keymap-alist)
(defvar embark-default-action-overrides)
(defvar embark-buffer-map)
(defvar embark-general-map)

(defmacro consult-rexel-test--with-requests (sent &rest body)
  "Run BODY with the requests collecting in SENT as (CONN METHOD PAYLOAD CALLBACK)."
  (declare (indent 1))
  `(let ((,sent nil))
     (cl-letf (((symbol-function 'rexel--request)
                (lambda (conn method payload &optional callback)
                  (push (list conn method payload callback) ,sent))))
       ,@body)))

(defun consult-rexel-test--answer (request &optional result err)
  "Answer REQUEST, an entry of the sent requests, with RESULT or ERR."
  (funcall (nth 3 request) result err))

(ert-deftest consult-rexel-test-session-names ()
  "A remote session carries its host, a repeated name the end of its ID."
  (let ((local (rexel--make-conn))
        (remote (rexel--make-conn :host "studio")))
    (should (equal `(("work" ,local . "session:aaaaaa")
                     ("work@studio" ,remote . "session:bbbbbb")
                     ("play@studio <cccccc>" ,remote . "session:cccccc")
                     ("play@studio <dddddd>" ,remote . "session:dddddd"))
                   (consult-rexel--name-sessions
                    `(("work" ,local . "session:aaaaaa")
                      ("work" ,remote . "session:bbbbbb")
                      ("play" ,remote . "session:cccccc")
                      ("play" ,remote . "session:dddddd")))))))

(ert-deftest consult-rexel-test-session-lookup ()
  "A listed session is found by name, on a ready connection unless told otherwise."
  (let* ((ready (rexel--make-conn :client-id t))
         (down (rexel--make-conn))
         (consult-rexel--sessions `(("one" ,ready . "s1") ("two" ,down . "s2"))))
    (should (equal (cons ready "s1") (consult-rexel--session "one")))
    (should-error (consult-rexel--session "two") :type 'user-error)
    (should (equal (cons down "s2") (consult-rexel--session "two" t)))
    (should-error (consult-rexel--session "three" t) :type 'user-error)))

(ert-deftest consult-rexel-test-list-sessions ()
  "The sessions of every connection are listed, a failing one left out."
  (let ((a (rexel--make-conn)) (b (rexel--make-conn :host "b"))
        (consult-rexel--sessions nil)
        (done 0))
    (consult-rexel-test--with-requests sent
      (cl-letf (((symbol-function 'run-at-time) (lambda (&rest _) (timer-create))))
        (consult-rexel--list-sessions (list a b) (lambda () (cl-incf done))))
      (should (equal '("session.attach_all" "session.attach_all")
                     (mapcar #'cadr sent)))
      (should (equal '((event_scope . "session")) (nth 2 (car sent))))
      (consult-rexel-test--answer
       (assq a sent) '((sessions . [((label . "one") (session_id . "session:1"))])))
      (should (= 0 done))
      (let ((inhibit-message t))
        (consult-rexel-test--answer (assq b sent) nil '((code . "internal"))))
      (should (= 1 done))
      (should (equal `(("one" ,a . "session:1")) consult-rexel--sessions)))))

(ert-deftest consult-rexel-test-block-pairs ()
  "Only the buffers of blocks are offered."
  (let ((block (generate-new-buffer "consult-rexel-test-block"))
        (other (generate-new-buffer "consult-rexel-test-other")))
    (unwind-protect
        (progn
          (with-current-buffer block (setq rexel--block-id "block:1"))
          (let ((buffers (mapcar #'cdr (consult-rexel--block-pairs))))
            (should (memq block buffers))
            (should-not (memq other buffers))))
      (kill-buffer block)
      (kill-buffer other))))

(ert-deftest consult-rexel-test-session-request ()
  "A request for a session follows the request that opens the sessions to it."
  (let ((session (cons (rexel--make-conn) "s")))
    (consult-rexel-test--with-requests sent
      (consult-rexel--session-request session "session.set_label" '((label . "x")))
      (should (equal '("session.attach_all") (mapcar #'cadr sent)))
      (consult-rexel-test--answer (car sent) '((sessions . [])))
      (should (equal '("session.set_label" ((session_id . "s") (label . "x")))
                     (seq-subseq (car sent) 1 3))))
    (consult-rexel-test--with-requests sent
      (consult-rexel--session-request session "session.destroy")
      (let ((inhibit-message t))
        (consult-rexel-test--answer (car sent) nil '((code . "internal"))))
      (should (= 1 (length sent))))))

(ert-deftest consult-rexel-test-session-preview ()
  "A session not attached is previewed by its focused block's text, fetched once."
  (let* ((conn (rexel--make-conn :client-id t))
         (attached (rexel--make-conn :client-id t :sessions '(("a" . "work"))))
         (consult-rexel--sessions `(("one" ,conn . "s") ("work" ,attached . "a")))
         (view '((active_window_id . "w")
                 (windows . [((window_id . "w") (focused_block_id . "b")
                              (layers . [((blocks . [((block_id . "b")
                                                      (creator_name
                                                       . "com.superlogical.terminal"))]))]))])))
         (state (consult-rexel--session-state))
         (buffer (window-buffer)))
    (consult-rexel-test--with-requests sent
      (funcall state 'preview "work")
      (should-not sent)
      (funcall state 'preview "one")
      (should (equal '("session.view" ((session_id . "s")))
                     (seq-subseq (car sent) 1 3)))
      (consult-rexel-test--answer (car sent) view)
      (consult-rexel-test--answer (car sent) '((content . "the text")))
      (should (equal "the text" (with-current-buffer (window-buffer)
                                  (buffer-string))))
      (funcall state 'preview nil)
      (should (eq buffer (window-buffer)))
      (funcall state 'preview "one")
      (should (= 2 (length sent)))
      (should (equal " *rexel-preview*" (buffer-name (window-buffer))))
      (funcall state 'preview nil)
      (funcall state 'exit nil)
      (funcall state 'return nil)
      (should-not (get-buffer " *rexel-preview*")))
    ;; A session without a tab has no preview.
    (let ((state (consult-rexel--session-state)))
      (consult-rexel-test--with-requests sent
        (funcall state 'preview "one")
        (consult-rexel-test--answer (car sent) '((windows . [])))
        (should (= 1 (length sent)))
        (funcall state 'return nil)))
    ;; An answer that comes after the picker closed shows nothing.
    (let ((state (consult-rexel--session-state)))
      (consult-rexel-test--with-requests sent
        (funcall state 'preview "one")
        (funcall state 'preview nil)
        (funcall state 'exit nil)
        (funcall state 'return nil)
        (consult-rexel-test--answer (car sent) view)
        (consult-rexel-test--answer (car sent) '((content . "late")))
        (should (eq buffer (window-buffer)))
        (should-not (get-buffer " *rexel-preview*"))))))

(ert-deftest consult-rexel-test-block-text ()
  "Text that fits comes in one reply; of longer text the end is read."
  (let ((consult-rexel--preview-bytes 100) text)
    (consult-rexel-test--with-requests sent
      (consult-rexel--block-text 'conn "s" "b" (lambda (s) (setq text s)))
      (should (equal 100 (alist-get 'max_bytes (alist-get 'args (nth 2 (car sent))))))
      (consult-rexel-test--answer (car sent) '((content . "all of it")))
      (should (equal "all of it" text))
      (should (= 1 (length sent))))
    (consult-rexel-test--with-requests sent
      (consult-rexel--block-text 'conn "s" "b" (lambda (s) (setq text s)))
      (consult-rexel-test--answer (car sent) '((content . "start") (id . 7)
                                               (total . 1000)))
      (should (equal "com.superlogical.terminal.blob_read" (cadr (car sent))))
      (should (equal '((id . 7) (offset . 900))
                     (alist-get 'args (nth 2 (car sent)))))
      (consult-rexel-test--answer (car sent) '((data . "cut line\nlast\nlines")))
      (should (equal "last\nlines" text)))
    ;; The size limit counts encoded bytes, so text within it can be cut.
    (consult-rexel-test--with-requests sent
      (consult-rexel--block-text 'conn "s" "b" (lambda (s) (setq text s)))
      (consult-rexel-test--answer (car sent) '((content . "first") (id . 7)
                                               (total . 90)))
      (should (equal '((id . 7) (offset . 0)) (alist-get 'args (nth 2 (car sent)))))
      (consult-rexel-test--answer (car sent) '((data . "first line\nlast")))
      (should (equal "first line\nlast" text)))))

(ert-deftest consult-rexel-test-layout-preview-is-undone ()
  "A tab is previewed in splits of the selected window, which are deleted again."
  (let* ((view `((windows . [((window_id . "w")
                              (layers . [((layout . ((block_id . "b"))))]))])))
         (tab (seq-first (alist-get 'windows view)))
         (buffer (window-buffer))
         (state (consult-rexel--choice-state (rexel--make-conn) "s" view))
         (split nil))
    (cl-letf (((symbol-function 'rexel--show-layout)
               (lambda (_conn _sid _layout _blocks window)
                 (setq split (split-window window nil 'right))
                 (set-window-buffer split (get-buffer-create "*Messages*"))
                 (set-window-buffer window (get-buffer-create "*Messages*")))))
      (delete-other-windows)
      (funcall state 'preview (cons 'tab tab))
      (should (= 2 (length (window-list nil 'no-mini))))
      (funcall state 'preview nil)
      (should (= 1 (length (window-list nil 'no-mini))))
      (should (eq buffer (window-buffer)))
      ;; A window made by something else during the preview stays.
      (funcall state 'preview (cons 'tab tab))
      (let ((other (split-window nil nil 'below)))
        (funcall state 'exit nil)
        (should (window-live-p other))
        (should-not (window-live-p split))
        (should (eq buffer (window-buffer)))
        (delete-window other))
      ;; The previewing window may be gone when the preview is undone.
      (let ((window (split-window nil nil 'below)))
        (with-selected-window window
          (funcall state 'preview (cons 'tab tab)))
        (delete-window window)
        (funcall state 'preview nil)
        (should-not (window-live-p split))
        (funcall state 'return nil))
      (set-window-dedicated-p (selected-window) t)
      (unwind-protect
          (progn
            (funcall state 'preview (cons 'tab tab))
            (should (= 1 (length (window-list nil 'no-mini)))))
        (set-window-dedicated-p (selected-window) nil)))))

(ert-deftest consult-rexel-test-embark ()
  "The Embark actions are added and taken away, a key bound meanwhile kept."
  (let ((embark-keymap-alist '((buffer embark-buffer-map)))
        (embark-default-action-overrides '((file . find-file)))
        (embark-buffer-map (make-sparse-keymap))
        (embark-general-map (make-sparse-keymap)))
    (consult-rexel--embark t)
    (consult-rexel--embark t)
    (should (eq consult-rexel-block-map (lookup-key embark-buffer-map "R")))
    (should (equal '(consult-rexel-session-map)
                   (alist-get 'rexel-session embark-keymap-alist)))
    (should (eq 'consult-rexel-attach-session
                (alist-get 'rexel-session embark-default-action-overrides)))
    (should (eq 'switch-to-buffer
                (alist-get '(buffer . consult-rexel) embark-default-action-overrides
                           nil nil #'equal)))
    (consult-rexel--embark nil)
    (should-not (lookup-key embark-buffer-map "R"))
    (should (equal '((buffer embark-buffer-map)) embark-keymap-alist))
    (should (equal '((file . find-file)) embark-default-action-overrides))
    (consult-rexel--embark t)
    (define-key embark-buffer-map "R" #'ignore)
    (consult-rexel--embark nil)
    (should (eq #'ignore (lookup-key embark-buffer-map "R")))))

(ert-deftest consult-rexel-test-block-action-refuses-other-buffers ()
  "An action on a block runs in its buffer and refuses any other buffer."
  (with-temp-buffer
    (should-error (consult-rexel--in-block (buffer-name) #'ignore)
                  :type 'user-error)
    (setq rexel--block-id "block:1")
    (let ((block (current-buffer)) ran)
      (with-temp-buffer
        (consult-rexel--in-block (buffer-name block)
                                 (lambda () (interactive) (setq ran (current-buffer)))))
      (should (eq block ran)))))

(ert-deftest consult-rexel-test-narrow-keys ()
  "The blocks narrow with `r', in `consult-buffer' with `x'."
  (should (eq ?r (plist-get consult-rexel-source :narrow)))
  (should (equal '(?r . "Rex") (plist-get consult-rexel-source-hidden :narrow)))
  (should (equal '(?x . "Rex")
                 (plist-get consult-rexel-buffer-source-hidden :narrow))))

(ert-deftest consult-rexel-test-mode ()
  "The mode adds its source and its prompt, and takes them away again."
  (let ((consult-buffer-sources '(consult-source-buffer))
        (rexel-read-choice-function #'rexel--read-choice)
        (consult-rexel-mode nil))
    (consult-rexel-mode 1)
    (should (equal '(consult-source-buffer consult-rexel-buffer-source-hidden)
                   consult-buffer-sources))
    (should (eq #'consult-rexel--read-choice rexel-read-choice-function))
    (consult-rexel-mode -1)
    (should (equal '(consult-source-buffer) consult-buffer-sources))
    (should (eq #'rexel--read-choice rexel-read-choice-function))
    ;; A prompt set by someone else meanwhile is left alone.
    (consult-rexel-mode 1)
    (setq rexel-read-choice-function #'ignore)
    (consult-rexel-mode -1)
    (should (eq #'ignore rexel-read-choice-function))))

(provide 'consult-rexel-test)
;;; consult-rexel-test.el ends here
