;;; rexel-project.el --- Rex sessions of projects -*- lexical-binding: t; -*-

;; Author: Daniel Kraus <daniel@kraus.my>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A project has one Rex session, labelled with the project's name, and a
;; terminal of the project is a tab of it.  The session of a project in
;; a TRAMP directory is on the Rex server of that machine, see
;; `rexel-tramp-hosts'.  `rexel-project' shows the
;; session and creates it in the project's root, `rexel-project-new'
;; opens another tab, and `rexel-project-kill' destroys the session with
;; the project's buffers.  `rexel-project-session-label' is for
;; `rexel-default-session-function', and makes `rexel' show the session
;; of the project a buffer is in.
;;
;;   (with-eval-after-load 'project
;;     (add-to-list 'project-switch-commands '(rexel-project "Rexel") t))
;;   (global-set-key [remap project-kill-buffers] #'rexel-project-kill)

;;; Code:

(require 'project)
(require 'rexel)

(defun rexel-project--label (project)
  "Return the label of PROJECT's Rex session, which is the project's name."
  (if (fboundp 'project-name)
      (project-name project)
    (file-name-nondirectory (directory-file-name (project-root project)))))

(defun rexel-project--with-session (project callback &optional running)
  "Call CALLBACK with the connection, ID and label of PROJECT's Rex session.
The ID is nil when the server has no session of that label.  The server
is that of the project's root, and RUNNING is as for
`rexel--connect-directory'."
  (let ((label (rexel-project--label project))
        (default-directory (project-root project)))
    (rexel--connect-directory
     (lambda (conn)
       (rexel--find-session
        conn label (lambda (sid) (funcall callback conn sid label))))
     running)))

;;;###autoload
(defun rexel-project-session-label ()
  "Return the label of the current project's Rex session, nil outside a project.
As `rexel-default-session-function' it makes `rexel' show the session of
the project a buffer is in.  In a TRAMP directory the project is found
only while TRAMP is connected to its machine."
  (let ((project (project-current)))
    (and project (rexel-project--label project))))

;;;###autoload
(defun rexel-project (&optional arg)
  "Show the Rex session of the current project, creating it.
The session is the one labelled with the project's name, on the server
`rexel' uses in the project's root.  ARG is as for `rexel', with two
\\[universal-argument] the same as one; a new shell starts in the
project's root.
To add this to `project-switch-commands':
  (add-to-list \\='project-switch-commands \\='(rexel-project \"Rexel\") t)"
  (interactive "P")
  (let* ((project (project-current t))
         (default-directory (project-root project)))
    (rexel--open-session (rexel-project--label project) arg)))

;;;###autoload
(defun rexel-project-new ()
  "Open a tab in the Rex session of the current project.
Like \\[universal-argument] `rexel-project'."
  (interactive)
  (rexel-project t))

;;;###autoload
(defun rexel-project-kill ()
  "Kill the current project's buffers and destroy its Rex session.
`project-kill-buffers' kills the buffers, asking first.  The session
goes when that was confirmed, unless a block of it was kept there.  A
session with blocks that run a command asks, and so does a project
without buffers to kill.  Without a running Rex server for the project
only the buffers are killed.
To use this in place of `project-kill-buffers':
  (global-set-key [remap project-kill-buffers] #\\='rexel-project-kill)"
  (interactive)
  (let* ((project (project-current t))
         (rexel--kill-declined nil)
         ;; Nil when the user declined, and the message when there was
         ;; no buffer to kill.
         (killed (project-kill-buffers))
         (kept rexel--kill-declined))
    (when killed
      (rexel-project--with-session
       project
       (lambda (conn sid label)
         (when (and sid (not (member sid kept)))
           (rexel--destroy-session conn sid label (stringp killed))))
       t))))

(provide 'rexel-project)
;;; rexel-project.el ends here
