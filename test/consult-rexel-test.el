;;; consult-rexel-test.el --- Tests for consult-rexel -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests of what needs no server and no minibuffer: the candidates, the
;; requests and the preview states, with `rexel--request' stubbed.  The
;; pickers themselves run in `consult--multi' and are tested live.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'consult-rexel)

;; The commands check for ghostel's native module, which these tests run
;; without.
(fset 'ghostel-ensure-module #'ignore)

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

(defmacro consult-rexel-test--with-blocks (specs &rest body)
  "Run BODY with a block buffer bound for each of SPECS.
A spec is (VAR CONN SESSION-ID); the buffers are made in that order."
  (declare (indent 1))
  `(let ,(mapcar (lambda (spec)
                   `(,(car spec) (generate-new-buffer
                                  ,(format "consult-rexel-test-%s" (car spec)))))
                 specs)
     (unwind-protect
         (progn
           ,@(mapcar (lambda (spec)
                       `(with-current-buffer ,(car spec)
                          (setq rexel--conn ,(nth 1 spec)
                                rexel--session-id ,(nth 2 spec)
                                rexel--block-id ,(format "block:%s" (car spec)))))
                     specs)
           ,@body)
       ,@(mapcar (lambda (spec) `(kill-buffer ,(car spec))) specs))))

(ert-deftest consult-rexel-test-session-sources ()
  "Each attached session is a source of its blocks, the project's session first."
  (let* ((local (rexel--make-conn :client-id t
                                  :sessions '(("s1" . "emacs") ("s2" . "proj"))))
         (remote (rexel--make-conn :client-id t :host "studio"
                                   :sessions '(("s3" . "blog") ("s4" . "proj"))))
         (down (rexel--make-conn :sessions '(("s5" . "away"))))
         (rexel--conns (list local remote down)))
    (consult-rexel-test--with-blocks ((blog remote "s3") (home local "s1")
                                      (one local "s2") (two local "s2")
                                      (far remote "s4"))
      (cl-letf (((symbol-function 'project-current)
                 (lambda (&rest _) '(transient . "/tmp/proj/"))))
        (let ((sources (consult-rexel-session-sources)))
          ;; A session without a block here is last, and one of a
          ;; connection being redialled is still listed.
          (should (equal '("proj" "blog@studio" "emacs" "proj@studio" "away")
                         (mapcar (lambda (s) (plist-get s :name)) sources)))
          (should (equal (list one two)
                         (sort (mapcar #'cdr (funcall (plist-get (car sources)
                                                                 :items)))
                               (lambda (a b)
                                 (string< (buffer-name a) (buffer-name b))))))
          (should (equal (list blog)
                         (mapcar #'cdr (funcall (plist-get (nth 1 sources)
                                                           :items)))))
          (dolist (source sources)
            (should (equal '(?r . "Rex") (plist-get source :narrow)))
            (should (eq 'buffer (plist-get source :category)))))))))

(defvar consult-rexel-test-source)
(defvar consult-ghostel-project-sources)

(ert-deftest consult-rexel-test-without-blocks ()
  "The Rex blocks leave the items of other sources, and their own source goes."
  (let ((conn (rexel--make-conn)))
    (consult-rexel-test--with-blocks ((mine conn "s1") (other conn "s2"))
      (with-temp-buffer
        (let* ((plain (current-buffer))
               (consult-rexel-test-source
                `( :name "Terminals"
                   :items ,(lambda ()
                             (list (cons "a" mine) (buffer-name other)
                                   (cons "c" plain) "no buffer"))))
               (static '(:name "New" :items ("Create")))
               (sources (list 'consult-rexel-test-source static
                              'consult-rexel-source-hidden
                              'consult-rexel-test-unbound)))
          (let ((kept (consult-rexel--without-blocks sources)))
            (should (equal '("Terminals" "New")
                           (mapcar (lambda (s) (plist-get s :name)) kept)))
            (should (equal (list (cons "c" plain) "no buffer")
                           (funcall (plist-get (car kept) :items))))
            (should (equal '("Create") (plist-get (nth 1 kept) :items)))))))))

(ert-deftest consult-rexel-test-sources-expand ()
  "A function among the sources stands for the sources it returns."
  (cl-letf (((symbol-function 'consult-rexel-test--two)
             (lambda () (list '(:name "a") '(:name "b")))))
    (should (equal '(consult-rexel-source (:name "a") (:name "b")
                                          consult-rexel-source-new)
                   (consult-rexel--sources
                    '(consult-rexel-source consult-rexel-test--two
                                           consult-rexel-source-new))))))

(ert-deftest consult-rexel-test-session-source-lists-what-has-no-block-here ()
  "A session is a candidate unless a group shows its blocks."
  (let* ((conn (rexel--make-conn :client-id t
                                 :sessions '(("s1" . "here") ("s3" . "empty"))))
         (consult-rexel--sessions `(("here" ,conn . "s1") ("there" ,conn . "s2")
                                    ("empty" ,conn . "s3"))))
    (consult-rexel-test--with-blocks ((block conn "s1"))
      (should (equal '("there" "empty")
                     (funcall (plist-get consult-rexel-session-source :items)))))))

(ert-deftest consult-rexel-test-project-sources ()
  "A new name goes to the project's session; the blocks leave the other sources."
  (let* ((conn (rexel--make-conn :client-id t :sessions '(("s1" . "proj"))))
         (consult-rexel--project (list conn "s1" "proj"))
         (opened nil))
    (consult-rexel-test--with-blocks ((mine conn "s1") (other conn "s2"))
      (let* ((consult-ghostel-project-sources
              `(( :name "Ghostel (Project)" :new ignore
                  :items ,(lambda () (list (cons "a" mine) (cons "b" other)
                                           "plain")))))
             (sources (consult--multi-enabled-sources
                       (consult-rexel--sources consult-rexel-project-sources))))
        (should (equal '("proj" "Ghostel (Project)" "New")
                       (mapcar (lambda (s) (plist-get s :name)) sources)))
        (should (equal '("plain")
                       (funcall (plist-get (aref sources 1) :items))))
        (cl-letf (((symbol-function 'rexel--open-session)
                   (lambda (&rest args) (setq opened args))))
          (pcase-let ((`(,name . ,source)
                       (consult--multi-lookup sources "logs" nil)))
            (funcall (plist-get source :new) name)))
        (should (equal '("proj" t "logs") opened))))))

(ert-deftest consult-rexel-test-new ()
  "A new name opens a labelled tab in the session of `rexel'."
  (let ((rexel-default-session "home")
        (rexel-default-session-function nil)
        calls)
    (cl-letf (((symbol-function 'rexel--open-session)
               (lambda (&rest args) (push (cons 'open args) calls)))
              ((symbol-function 'rexel)
               (lambda (&optional arg) (push (list 'rexel arg) calls)))
              ((symbol-function 'rexel-new-session)
               (lambda (&rest _) (push '(session) calls))))
      (funcall (plist-get consult-rexel-session-source :new) "logs")
      (funcall (plist-get consult-rexel-source-new :action) "New Rex tab")
      (funcall (plist-get consult-rexel-source-new :action) "New Rex session")
      (let ((rexel-default-session-function (lambda () "proj")))
        (funcall (plist-get consult-rexel-session-source :new) "logs")))
    (should (equal '((open "proj" t "logs")
                     (session) (rexel t) (open "home" t "logs"))
                   calls))))

(ert-deftest consult-rexel-test-project ()
  "The project picker attaches the project's session before it lists it."
  (pcase-dolist (`(,sid ,attached ,attaches)
                 '(("s1" nil t) ("s1" t nil) (nil nil nil)))
    (let* ((conn (rexel--make-conn :client-id t
                                   :sessions (and attached '(("s1" . "proj")))))
           (attached-to nil) (picked nil) (opened nil))
      (cl-letf (((symbol-function 'project-current)
                 (lambda (&rest _) '(transient . "/tmp/proj/")))
                ((symbol-function 'rexel--connect-directory)
                 (lambda (callback &rest _)
                   (should (equal "/tmp/proj/" default-directory))
                   (funcall callback conn)))
                ((symbol-function 'rexel--find-session)
                 (lambda (_conn label callback)
                   (should (equal "proj" label))
                   (funcall callback sid)))
                ((symbol-function 'rexel--attach)
                 (lambda (_conn sid callback)
                   (setq attached-to sid)
                   (funcall callback nil)))
                ((symbol-function 'rexel--prompt-later) #'funcall)
                ((symbol-function 'rexel--open-session)
                 (lambda (&rest args) (setq opened args)))
                ((symbol-function 'consult--multi)
                 (lambda (sources &rest _)
                   (setq picked (list consult-rexel--project default-directory
                                      (plist-get (car sources) :name)))
                   (funcall (plist-get (car sources) :new) "logs"))))
        (let ((default-directory "/tmp/proj/src/")
              (consult-rexel-project-sources
               '(consult-rexel-project-session-sources
                 consult-rexel-project-source-new)))
          (consult-rexel-project)))
      (should (equal (and attaches sid) attached-to))
      (should (equal (list conn sid "proj") (car picked)))
      (should (equal "/tmp/proj/" (nth 1 picked)))
      (should (equal "proj" (nth 2 picked)))
      (should (equal '("proj" t "logs") opened)))))

(ert-deftest consult-rexel-test-picker ()
  "The picker connects when no server is, and opens in the caller's buffer."
  (pcase-dolist (ready '(t nil))
    (let* ((conn (rexel--make-conn :client-id ready))
           (rexel--conns (list conn))
           connected listed picked)
      (cl-letf (((symbol-function 'rexel--read-host) (lambda () 'host))
                ((symbol-function 'rexel--connect)
                 (lambda (host callback)
                   (setq connected host)
                   (setf (rexel--conn-client-id conn) t)
                   (funcall callback conn)))
                ((symbol-function 'consult-rexel--list-sessions)
                 (lambda (conns callback)
                   (setq listed conns)
                   (funcall callback)))
                ((symbol-function 'rexel--prompt-later)
                 (lambda (function) (with-temp-buffer (funcall function))))
                ((symbol-function 'consult--multi)
                 (lambda (&rest _) (setq picked (current-buffer)))))
        (with-temp-buffer
          (consult-rexel)
          (should (eq (current-buffer) picked))))
      (should (eq (if ready nil 'host) connected))
      (should (equal (list conn) listed)))))

(ert-deftest consult-rexel-test-prefix-argument ()
  "With a prefix argument the picker is `rexel'."
  (let (called)
    (cl-letf (((symbol-function 'rexel)
               (lambda (&optional arg) (setq called (list arg))))
              ((symbol-function 'rexel--connect)
               (lambda (&rest _) (error "Connected")))
              ((symbol-function 'consult-rexel--list-sessions)
               (lambda (&rest _) (error "Listed the sessions"))))
      (consult-rexel '(4)))
    (should (equal '((4)) called))))

(ert-deftest consult-rexel-test-commands-need-the-native-module ()
  "Without ghostel's native module the pickers end before they connect."
  (cl-letf (((symbol-function 'ghostel-ensure-module)
             (lambda () (user-error "No module")))
            ((symbol-function 'project-current)
             (lambda (&rest _) (ert-fail "looked for the project")))
            ((symbol-function 'rexel--connect)
             (lambda (&rest _) (ert-fail "connected")))
            ((symbol-function 'consult-rexel--list-sessions)
             (lambda (&rest _) (ert-fail "listed the sessions")))
            ((symbol-function 'consult-rexel--session)
             (lambda (&rest _) (ert-fail "looked up the session"))))
    (should-error (consult-rexel) :type 'user-error)
    (should-error (consult-rexel-project) :type 'user-error)
    (should-error (consult-rexel-attach-session "work") :type 'user-error)))

(ert-deftest consult-rexel-test-project-prefix-argument ()
  "With a prefix argument the project picker is `rexel-project'."
  (let (called)
    (cl-letf (((symbol-function 'rexel-project)
               (lambda (&optional arg) (setq called (list arg))))
              ((symbol-function 'consult--multi)
               (lambda (&rest _) (error "Opened the picker"))))
      (consult-rexel-project '(4)))
    (should (equal '((4)) called))))

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
