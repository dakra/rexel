;;; rexel-project-test.el --- Tests for rexel-project -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests of the project commands with the project, the connection and the
;; requests stubbed.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'rexel-project)

(defmacro rexel-project-test--with-project (root &rest body)
  "Run BODY with ROOT the root of the current project."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'project-current)
              (lambda (&rest _) (cons 'transient ,root))))
     ,@body))

(ert-deftest rexel-project-test-shows-the-session-named-after-the-project ()
  "The session is labelled with the project's name; shells start in its root."
  (let (opened)
    (cl-letf (((symbol-function 'rexel--open-session)
               (lambda (label &optional arg)
                 (push (list label arg default-directory) opened))))
      (rexel-project-test--with-project "/tmp/proj/"
        (let ((default-directory "/tmp/proj/src/"))
          (rexel-project)
          (rexel-project 2)
          (rexel-project-new))
        (let ((rexel-project-label-function (lambda (project)
                                              (concat "p:" (project-root project)))))
          (rexel-project))))
    (should (equal '(("p:/tmp/proj/" nil "/tmp/proj/")
                     ("proj" t "/tmp/proj/")
                     ("proj" 2 "/tmp/proj/")
                     ("proj" nil "/tmp/proj/"))
                   opened))))

(defmacro rexel-project-test--with-session (destroyed &rest body)
  "Run BODY with this machine's server holding session \"sid\" of the project.
DESTROYED collects the arguments `rexel--destroy-session' gets after its
connection."
  (declare (indent 1))
  `(let* ((conn (rexel--make-conn :endpoints (list "sock") :client-id t))
          (rexel--conns (list conn))
          (,destroyed nil))
     (cl-letf (((symbol-function 'rexel--find-session)
                (lambda (_conn label callback)
                  (funcall callback (and (equal label "proj") "sid"))))
               ((symbol-function 'rexel--destroy-session)
                (lambda (_conn &rest args) (push args ,destroyed))))
       (rexel-project-test--with-project "/tmp/proj/"
         ,@body))))

(ert-deftest rexel-project-test-kill-destroys-the-session ()
  "The session goes once `project-kill-buffers' killed the buffers."
  (rexel-project-test--with-session destroyed
    (cl-letf (((symbol-function 'project-kill-buffers) #'ignore))
      (rexel-project-kill)
      (should-not destroyed))
    (cl-letf (((symbol-function 'project-kill-buffers) (lambda () '(killed))))
      (rexel-project-kill)
      (should (equal '(("sid" "proj" nil)) destroyed)))))

(ert-deftest rexel-project-test-kill-asks-without-buffers ()
  "A project without buffers to kill asks for its session alone."
  (rexel-project-test--with-session destroyed
    (cl-letf (((symbol-function 'project-kill-buffers)
               (lambda () "No buffers to kill")))
      (rexel-project-kill))
    (should (equal '(("sid" "proj" t)) destroyed))))

(ert-deftest rexel-project-test-kill-keeps-the-session-of-a-kept-block ()
  "A block of the session kept while the buffers were killed keeps the session."
  (dolist (session '("sid" "other"))
    (rexel-project-test--with-session destroyed
      (with-temp-buffer
        (setq rexel--session-id session)
        (let ((kept (current-buffer)))
          (cl-letf (((symbol-function 'project-kill-buffers)
                     (lambda () (push kept rexel--kill-declined) '(killed))))
            (rexel-project-kill))))
      (should (eq (equal session "other") (and destroyed t))))))

(ert-deftest rexel-project-test-kill-starts-no-server ()
  "Without a running server only the buffers are killed."
  (let ((rexel--conns nil)
        (killed nil))
    (cl-letf (((symbol-function 'rexel--hosts) #'ignore)
              ((symbol-function 'rexel--autostart-server)
               (lambda () (error "Offered to start a server")))
              ((symbol-function 'rexel--connect)
               (lambda (&rest _) (error "Dialled a server")))
              ((symbol-function 'project-kill-buffers)
               (lambda () (setq killed t) '(killed))))
      (rexel-project-test--with-project "/tmp/proj/"
        (rexel-project-kill)))
    (should killed)))

(ert-deftest rexel-project-test-kill-of-a-remote-project-without-a-server ()
  "A project on a machine without a known Rex server loses its buffers alone."
  (let ((rexel--conns nil)
        (rexel-tramp-hosts nil)
        (killed nil))
    (cl-letf (((symbol-function 'rexel--hosts) (lambda () '((nil "sock"))))
              ((symbol-function 'rexel--connect)
               (lambda (&rest _) (error "Dialled a server")))
              ((symbol-function 'project-kill-buffers)
               (lambda () (setq killed t) '(killed))))
      (rexel-project-test--with-project "/ssh:box:/srv/app/"
        (let ((default-directory "/ssh:box:/srv/app/src/"))
          (rexel-project-kill))))
    (should killed)))

(ert-deftest rexel-project-test-kill-uses-the-server-of-the-project ()
  "The session's server is the project's, also once the current buffer is killed."
  (let ((file (generate-new-buffer "rexel-project-test-file"))
        (other (generate-new-buffer "rexel-project-test-other"))
        directory)
    (unwind-protect
        (cl-letf (((symbol-function 'rexel--connect-directory)
                   (lambda (&rest _) (setq directory default-directory)))
                  ((symbol-function 'project-kill-buffers)
                   (lambda ()
                     (kill-buffer file)
                     (set-buffer other)
                     '(killed))))
          (with-current-buffer other (setq default-directory "/tmp/"))
          (rexel-project-test--with-project "/ssh:box:/srv/app/"
            (save-current-buffer
              (set-buffer file)
              (setq default-directory "/ssh:box:/srv/app/src/")
              (rexel-project-kill)))
          (should (equal "/ssh:box:/srv/app/" directory)))
      (when (buffer-live-p file) (kill-buffer file))
      (kill-buffer other))))

(provide 'rexel-project-test)
;;; rexel-project-test.el ends here
