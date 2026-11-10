;;; rexel-project.el --- Rex sessions of projects -*- lexical-binding: t; -*-

;; Author: Daniel Kraus <daniel@kraus.my>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A project has one Rex session, labelled with the project's name, and a
;; terminal of the project is a tab of it.  `rexel-project' shows the
;; session and creates it in the project's root, `rexel-project-new'
;; opens another tab, and `rexel-project-kill' destroys the session with
;; the project's buffers.
;;
;;   (with-eval-after-load 'project
;;     (add-to-list 'project-switch-commands '(rexel-project "Rexel") t))
;;   (global-set-key [remap project-kill-buffers] #'rexel-project-kill)

;;; Code:

(require 'project)
(require 'rexel)

(defcustom rexel-project-label-function #'rexel-project-name
  "Function returning the label of a project's Rex session.
It receives the project.  Projects with one label share a session."
  :type 'function
  :group 'rexel)

(defun rexel-project-name (project)
  "Return the name of PROJECT, by default the last part of its root."
  (if (fboundp 'project-name)
      (project-name project)
    (file-name-nondirectory (directory-file-name (project-root project)))))

;;;###autoload
(defun rexel-project (&optional arg)
  "Show the Rex session of the current project, creating it.
The session is the one `rexel-project-label-function' names, on the
server `rexel' uses.  ARG is as for `rexel'; a new shell starts in the
project's root.
To add this to `project-switch-commands':
  (add-to-list \\='project-switch-commands \\='(rexel-project \"Rexel\") t)"
  (interactive "P")
  (let* ((project (project-current t))
         (default-directory (project-root project)))
    (rexel--open-session (funcall rexel-project-label-function project) arg)))

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
without buffers to kill.  Without a running Rex server only the buffers
are killed.
To use this in place of `project-kill-buffers':
  (global-set-key [remap project-kill-buffers] #\\='rexel-project-kill)"
  (interactive)
  (let* ((label (funcall rexel-project-label-function (project-current t)))
         (rexel--kill-declined nil)
         ;; Nil when the user declined, and the message when there was
         ;; no buffer to kill.
         (killed (project-kill-buffers))
         (declined rexel--kill-declined))
    (when killed
      (rexel--connect-local
       (lambda (conn)
         (rexel--find-session
          conn label
          (lambda (sid)
            (when (and sid
                       (not (seq-some
                             (lambda (buf)
                               (and (buffer-live-p buf)
                                    (equal (buffer-local-value
                                            'rexel--session-id buf)
                                           sid)))
                             declined)))
              (rexel--destroy-session conn sid label (stringp killed))))))
       t))))

(provide 'rexel-project)
;;; rexel-project.el ends here
