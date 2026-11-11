;;; consult-rexel.el --- Consult integration for rexel -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Daniel Kraus <daniel@kraus.my>

;; Author: Daniel Kraus <daniel@kraus.my>
;; URL: https://github.com/dakra/rexel
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1") (consult "1.5") (rexel "0.2.0"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Pick Rex blocks and sessions through `consult'.
;;
;; `consult-rexel' lists the block buffers, grouped by their sessions,
;; the other sessions of the connected servers, and with consult-ghostel
;; installed what `consult-ghostel' lists.  A block is
;; previewed as a buffer and a session by the text of its focused block.
;; Choosing a session attaches it, and a name that matches nothing opens
;; a tab with that label in the session of `rexel'.  Listing the sessions
;; subscribes the connection to the session events of its whole server.
;;
;; `consult-rexel-project' does the same for the session of the current
;; project and what `consult-ghostel-project' lists.
;;
;; `consult-rexel-tab' picks a tab of a session and rebuilds the frame's
;; windows from its layout, previewing each layout.
;;
;; `consult-rexel-mode' adds the blocks as a hidden source to
;; `consult-ghostel' (narrow key `r') and to `consult-buffer' (`x', as
;; `r' shows the registers there), and gives the prompts of
;; `rexel-attach' and `rexel-sync-layout' the same previews.  With Embark
;; installed it adds the actions of `consult-rexel-session-map' and,
;; behind `consult-rexel-embark-key', of `consult-rexel-block-map'.
;;
;;   (use-package consult-rexel
;;     :hook (after-init . consult-rexel-mode)
;;     :bind (("C-x m" . consult-rexel)
;;            :map project-prefix-map
;;            ("m" . consult-rexel-project)))

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'consult)
(require 'rexel)
(require 'rexel-project)
(require 'consult-ghostel nil 'noerror)
(require 'embark nil 'noerror)

(defvar consult-ghostel-sources)
(defvar consult-ghostel-project-sources)
(defvar embark-keymap-alist)
(defvar embark-default-action-overrides)
(defvar embark-buffer-map)
(defvar embark-general-map)

(defvar consult-rexel--history nil
  "Minibuffer history of `consult-rexel'.")

(defvar consult-rexel--sessions nil
  "The sessions `consult-rexel' last listed, as (NAME CONN . SESSION-ID).")

(defvar consult-rexel--preview-bytes 16384
  "Bytes of a block's text fetched to preview a session.")


;;; Blocks

(defun consult-rexel--block-p (buffer)
  "Return non-nil if BUFFER shows a Rex block."
  (buffer-local-value 'rexel--block-id buffer))

(defun consult-rexel--block-pairs ()
  "Return `consult' (NAME . BUFFER) pairs of the block buffers, switch-ordered."
  (consult--buffer-query :sort 'visibility
                         :predicate #'consult-rexel--block-p
                         :as #'consult--buffer-pair))

(defvar consult-rexel-source
  `( :name     "Rex"
     :narrow   ?r
     :category buffer
     :face     consult-buffer
     :history  buffer-name-history
     :annotate ,#'ghostel-annotate-buffer
     :state    ,#'consult--buffer-state
     :new      ,#'rexel-new-session
     :items    ,#'consult-rexel--block-pairs)
  "`consult' source for the buffers of Rex blocks.")

;; `copy-sequence', or `consult-customize' on one variable would change
;; the other through the shared tail.
(defvar consult-rexel-source-hidden
  `(:hidden t :narrow (?r . "Rex") ,@(copy-sequence consult-rexel-source))
  "Like `consult-rexel-source' but hidden until narrowed to.
`consult-rexel-mode' adds it to `consult-ghostel-sources'.")

(defvar consult-rexel-buffer-source-hidden
  `(:hidden t :narrow (?x . "Rex") ,@(copy-sequence consult-rexel-source))
  "Like `consult-rexel-source-hidden' but narrowed to with `x'.
`consult-rexel-mode' adds it to `consult-buffer-sources', whose
register sources narrow with `r'.")

;;; Blocks by session

(defun consult-rexel--session-name (conn label)
  "Return LABEL, the label of a session on CONN, with the host of a remote one."
  (if (rexel--conn-host conn)
      (format "%s@%s" label (rexel--conn-host conn))
    label))

(defun consult-rexel--session-block-p (conn sid buffer)
  "Return non-nil if BUFFER shows a block of session SID on CONN."
  (and (eq (buffer-local-value 'rexel--conn buffer) conn)
       (equal (buffer-local-value 'rexel--session-id buffer) sid)
       (consult-rexel--block-p buffer)))

(defun consult-rexel--session-source (conn sid name new)
  "Return a `consult' source named NAME for the blocks of session SID on CONN.
NEW is called with a name that matches no candidate."
  `( :name     ,name
     :narrow   (?r . "Rex")
     :category buffer
     :face     consult-buffer
     :history  buffer-name-history
     :annotate ,#'ghostel-annotate-buffer
     :state    ,#'consult--buffer-state
     :new      ,new
     :items    ,(lambda ()
                  (consult--buffer-query
                   :sort 'visibility
                   :predicate (apply-partially #'consult-rexel--session-block-p
                                               conn sid)
                   :as #'consult--buffer-pair))))

(defun consult-rexel--new-tab (label)
  "Open a tab labelled LABEL in the session of `rexel'."
  (rexel--open-session (rexel--default-session) t label))

(defun consult-rexel-session-sources ()
  "Return a `consult' source of the blocks of each attached session.
The session of the current project comes first, then the sessions in
the order their blocks were selected last."
  (let ((project (and (not (file-remote-p default-directory))
                      (rexel-project-session-label)))
        (blocks (seq-filter #'consult-rexel--block-p (buffer-list)))
        (sessions nil))
    (dolist (conn rexel--conns)
      (pcase-dolist (`(,sid . ,label) (rexel--conn-sessions conn))
        (push (list (cond ((and (equal label project)
                                (null (rexel--conn-host conn)))
                           -1)
                          ((seq-position
                            blocks nil
                            (lambda (buffer _)
                              (consult-rexel--session-block-p
                               conn sid buffer))))
                          (t (length blocks)))
                    conn sid (consult-rexel--session-name conn label))
              sessions)))
    (mapcar (pcase-lambda (`(,_ ,conn ,sid ,name))
              (consult-rexel--session-source conn sid name
                                             #'consult-rexel--new-tab))
            (sort (nreverse sessions) (lambda (a b) (< (car a) (car b)))))))

(defun consult-rexel--item-buffer (item)
  "Return the buffer of the `consult' candidate ITEM, or nil."
  (let ((buffer (if (consp item) (cdr item) item)))
    (if (stringp buffer) (get-buffer buffer) (and (bufferp buffer) buffer))))

(defun consult-rexel--without-blocks (sources &optional predicate)
  "Return the `consult' SOURCES with the Rex blocks left out of their items.
With PREDICATE, leave out only the blocks whose buffer it accepts.  The
sources that list the Rex blocks alone are left out."
  (delq nil
        (mapcar
         (lambda (source)
           (let* ((plist (if (symbolp source)
                             (and (boundp source) (symbol-value source))
                           source))
                  (items (plist-get plist :items)))
             (cond ((eq items #'consult-rexel--block-pairs) nil)
                   ((functionp items)
                    `(:items
                      ,(lambda ()
                         (seq-remove
                          (lambda (item)
                            (when-let* ((buffer (consult-rexel--item-buffer item)))
                              (funcall (or predicate #'consult-rexel--block-p)
                                       buffer)))
                          (funcall items)))
                      ,@plist))
                   (t plist))))
         sources)))

(defun consult-rexel-ghostel-sources ()
  "Return the sources of `consult-ghostel' without the Rex blocks."
  (consult-rexel--without-blocks (bound-and-true-p consult-ghostel-sources)))

;;; Sessions

(defun consult-rexel--session (name &optional offline)
  "Return (CONN . SESSION-ID) of the listed session NAME.
Unless OFFLINE, its connection must be ready."
  (let ((session (or (cdr (assoc name consult-rexel--sessions))
                     (user-error "No Rex session %s in the last list" name))))
    (unless (or offline (rexel--ready-p (car session)))
      (user-error "No connection to the server of Rex session %s" name))
    session))

(defun consult-rexel--attached-p (session)
  "Return non-nil if SESSION, a (CONN . SESSION-ID), is attached."
  (assoc (cdr session) (rexel--conn-sessions (car session))))

(defun consult-rexel--name-sessions (sessions)
  "Return SESSIONS, a list of (LABEL CONN . SESSION-ID), under unique names.
A session of a remote host is named LABEL@HOST, and a name that repeats
gets the end of its ID appended, as `rex ls' shows it."
  (let ((named (mapcar (pcase-lambda (`(,label ,conn . ,sid))
                         (cons (consult-rexel--session-name conn label)
                               (cons conn sid)))
                       sessions)))
    (mapcar (lambda (entry)
              (if (cdr (seq-filter (lambda (o) (equal (car o) (car entry))) named))
                  (cons (format "%s <%s>" (car entry) (substring (cddr entry) -6))
                        (cdr entry))
                entry))
            named)))

(defconst consult-rexel--attach-all
  '("session.attach_all" ((event_scope . "session")))
  "The request that lists a server's sessions and opens them to requests.
Without it a request naming a session the connection is not attached to
fails.  It also subscribes to every session's events but `block_event'.")

(defun consult-rexel--list-sessions (conns callback)
  "Call CALLBACK once CONNS listed their sessions, or after 1.5 seconds.
The sessions go to `consult-rexel--sessions'."
  (let ((pending (length conns))
        (sessions nil)
        (timer nil))
    (cl-flet ((finish ()
                (when timer
                  (cancel-timer timer)
                  (setq timer nil
                        consult-rexel--sessions
                        (consult-rexel--name-sessions (nreverse sessions)))
                  (funcall callback))))
      (setq timer (run-at-time 1.5 nil #'finish))
      (dolist (conn conns)
        (apply #'rexel--request conn
               `(,@consult-rexel--attach-all
                 ,(lambda (result err)
                    (if err
                        (message "rexel: session.attach_all: %s"
                                 (rexel--error-string err))
                      (seq-doseq (s (alist-get 'sessions result))
                        (push (cons (alist-get 'label s)
                                    (cons conn (alist-get 'session_id s)))
                              sessions)))
                    (when (zerop (cl-decf pending))
                      (finish)))))))))

(defun consult-rexel--shown-p (session)
  "Return non-nil if SESSION, a (CONN . SESSION-ID), has a block buffer."
  (seq-some (apply-partially #'consult-rexel--session-block-p
                             (car session) (cdr session))
            (buffer-list)))

(defun consult-rexel-attach-session (name)
  "Attach the listed session NAME and show it as `rexel-attach-display' says."
  (interactive (list (consult-rexel--read-session "Attach Rex session: ")))
  (ghostel-ensure-module)
  (pcase-let ((`(,conn . ,sid) (consult-rexel--session name)))
    (if (consult-rexel--attached-p (cons conn sid))
        (rexel--with-view
         conn sid
         (lambda (view)
           (rexel--show-attached conn sid view rexel-attach-display)))
      (rexel--attach-session conn sid rexel-attach-display))))

(defun consult-rexel--block-text (conn sid bid callback)
  "Call CALLBACK with the end of the text of block BID in session SID on CONN.
At most `consult-rexel--preview-bytes' are fetched.  CALLBACK is not
called on an error."
  (let ((params `((session_id . ,sid) (block_id . ,bid)))
        (offset nil))
    (rexel--request
     conn "com.superlogical.terminal.format"
     `(,@params (args . ((format . "text") (trim . t)
                         (max_bytes . ,consult-rexel--preview-bytes))))
     (lambda (result err)
       (cond
        (err)
        ((null (alist-get 'id result))
         (funcall callback (alist-get 'content result)))
        (t
         (setq offset (max 0 (- (alist-get 'total result)
                                consult-rexel--preview-bytes)))
         (rexel--request
          conn "com.superlogical.terminal.blob_read"
          `(,@params (args . ((id . ,(alist-get 'id result))
                              (offset . ,offset))))
          (lambda (result err)
            (unless err
              (funcall callback
                       (if (zerop offset)
                           (alist-get 'data result)
                         ;; The text starts within a line.
                         (replace-regexp-in-string
                          "\\`[^\n]*\n" "" (alist-get 'data result)))))))))))))

(defun consult-rexel--session-state ()
  "Return a `consult' state that previews sessions that are not attached.
The preview is the end of the text of the session's focused block."
  (let ((preview (consult--buffer-preview))
        (buffer nil)
        (texts nil)
        (current nil))
    (cl-labels
        ((show (name)
           (when (equal name current)
             (unless (buffer-live-p buffer)
               (setq buffer (generate-new-buffer " *rexel-preview*")))
             (with-current-buffer buffer
               (erase-buffer)
               (insert (cdr (assoc name texts))))
             (with-selected-window (or (minibuffer-selected-window)
                                       (selected-window))
               (funcall preview 'preview buffer))
             (when-let* ((window (get-buffer-window buffer)))
               (with-selected-window window
                 (goto-char (point-max))
                 (recenter -1)))))
         (fetch (name conn sid)
           (push (cons name nil) texts)
           (rexel--request
            conn "session.view" `((session_id . ,sid))
            (lambda (view err)
              (unless err
                (let ((tab (or (rexel--active-tab view)
                               (seq-find #'identity (alist-get 'windows view)))))
                  (when-let* ((bid (and tab (rexel--tab-block tab))))
                    (consult-rexel--block-text
                     conn sid bid
                     (lambda (text)
                       (setcdr (assoc name texts) text)
                       (show name))))))))))
      (lambda (action name)
        (pcase action
          ('preview
           (setq current name)
           (let ((session (cdr (assoc name consult-rexel--sessions))))
             (cond
              ((cdr (assoc name texts)) (show name))
              (t
               (funcall preview 'preview nil)
               (when (and session
                          (rexel--ready-p (car session))
                          (not (consult-rexel--attached-p session))
                          (not (assoc name texts)))
                 (fetch name (car session) (cdr session)))))))
          ('exit
           (setq current nil)
           (funcall preview 'exit nil))
          ('return
           (funcall preview 'return nil)
           (when (buffer-live-p buffer)
             (kill-buffer buffer))))))))

(defvar consult-rexel-session-source
  `( :name     "Session"
     :narrow   ?s
     :category rexel-session
     :face     font-lock-keyword-face
     :state    ,#'consult-rexel--session-state
     :action   ,#'consult-rexel-attach-session
     :new      ,#'consult-rexel--new-tab
     :items    ,(lambda ()
                  (mapcar #'car (seq-remove
                                 (lambda (entry)
                                   (consult-rexel--shown-p (cdr entry)))
                                 consult-rexel--sessions))))
  "`consult' source for the sessions that have no block buffer.
They are those of the connected servers.")

(defvar consult-rexel-source-new
  `( :name     "New"
     :narrow   ?n
     :category rexel-new
     :face     font-lock-constant-face
     :action   ,(lambda (candidate)
                  (if (equal candidate "New Rex session")
                      (rexel-new-session)
                    (rexel t)))
     :items    ("New Rex tab" "New Rex session"))
  "`consult' source offering a tab in the session of `rexel', and a session.
The server chooses the session's label.")

(defcustom consult-rexel-sources
  '(consult-rexel-session-sources consult-rexel-session-source
    consult-rexel-ghostel-sources consult-rexel-source-new)
  "Sources of `consult-rexel'.
Each is a source as in `consult-buffer-sources', or a function that
returns a list of sources."
  :type '(repeat symbol)
  :group 'rexel)

(defun consult-rexel--display (buffer &optional norecord)
  "Pop to BUFFER as `rexel' and ghostel's commands show a terminal.
NORECORD is passed on to `pop-to-buffer'."
  (pop-to-buffer buffer rexel--same-window norecord))

(defun consult-rexel--sources (sources)
  "Return SOURCES with each function replaced by the sources it returns."
  (mapcan (lambda (source)
            (if (and (symbolp source) (not (boundp source)) (fboundp source))
                (copy-sequence (funcall source))
              (list source)))
          sources))

;;;###autoload
(defun consult-rexel (&optional arg)
  "Switch to a Rex block or attach a Rex session, previewing candidates.
The blocks are grouped by their sessions, and the sessions listed are
those of the connected servers without a block buffer.  With
consult-ghostel installed, the candidates of `consult-ghostel' follow.
Without a connection, connect to a server first, asking which if there
are several.  A name that matches nothing opens a tab with that label in
the session of `rexel'.
With prefix ARG, behave like `rexel' called with ARG."
  (interactive "P")
  (if arg
      (rexel arg)
    (ghostel-ensure-module)
    (let* ((conns (seq-filter #'rexel--ready-p rexel--conns))
           (buffer (current-buffer))
           (pick (lambda ()
                   (with-current-buffer (if (buffer-live-p buffer)
                                            buffer
                                          (current-buffer))
                     (let ((this-command 'consult-rexel)
                           (consult--buffer-display #'consult-rexel--display))
                       (consult--multi
                        (consult-rexel--sources consult-rexel-sources)
                        :prompt "Rex block or session: "
                        :require-match (confirm-nonexistent-file-or-buffer)
                        :history 'consult-rexel--history
                        :sort nil))))))
      (if conns
          (consult-rexel--list-sessions
           conns (lambda () (rexel--prompt-later pick)))
        (rexel--connect
         (rexel--read-host)
         (lambda (_conn)
           (consult-rexel--list-sessions
            (seq-filter #'rexel--ready-p rexel--conns)
            (lambda () (rexel--prompt-later pick)))))))))


;;; The session of a project

(defvar consult-rexel--project nil
  "The (CONN SESSION-ID LABEL) `consult-rexel-project' picks from.
SESSION-ID is nil when the server has no session for the project.")

(defun consult-rexel-project-session-sources ()
  "Return the `consult' source of the blocks in the current project's session."
  (pcase-let ((`(,conn ,sid ,label) consult-rexel--project))
    (list (consult-rexel--session-source
           conn sid label
           (lambda (name) (rexel--open-session label t name))))))

(defun consult-rexel-ghostel-project-sources ()
  "Return the sources of `consult-ghostel-project'.
The blocks of the project's session are left out of them."
  (pcase-let ((`(,conn ,sid) consult-rexel--project))
    (consult-rexel--without-blocks
     (bound-and-true-p consult-ghostel-project-sources)
     (apply-partially #'consult-rexel--session-block-p conn sid))))

(defvar consult-rexel-project-source-new
  `( :name     "New"
     :narrow   ?n
     :category rexel-new
     :face     font-lock-constant-face
     :action   ,(lambda (_) (rexel-project t))
     :items    ("New Rex tab"))
  "`consult' source offering a tab in the session of the current project.")

(defcustom consult-rexel-project-sources
  '(consult-rexel-project-session-sources consult-rexel-ghostel-project-sources
    consult-rexel-project-source-new)
  "Sources of `consult-rexel-project', as in `consult-rexel-sources'."
  :type '(repeat symbol)
  :group 'rexel)

;;;###autoload
(defun consult-rexel-project (&optional arg)
  "Switch to a block of the current project's Rex session, previewing candidates.
The session is that of `rexel-project', and is attached to first.  With
consult-ghostel installed, the candidates of `consult-ghostel-project'
follow.  A name that matches nothing opens a tab with that label in the
session, which is created if the server has none.
With prefix ARG, behave like `rexel-project' called with ARG."
  (interactive "P")
  (if arg
      (rexel-project arg)
    (ghostel-ensure-module)
    (let* ((project (project-current t))
           (root (project-root project)))
      (rexel-project--with-session
       project
       (lambda (conn sid label)
         (let ((pick
                (lambda (&rest _)
                  (rexel--prompt-later
                   (lambda ()
                     (let ((this-command 'consult-rexel-project)
                           (consult--buffer-display #'consult-rexel--display)
                           (default-directory root)
                           (consult-rexel--project (list conn sid label)))
                       (consult--multi
                        (consult-rexel--sources consult-rexel-project-sources)
                        :prompt "Project Rex block: "
                        :require-match (confirm-nonexistent-file-or-buffer)
                        :history 'consult-rexel--history
                        :sort nil)))))))
           (if (and sid (not (assoc sid (rexel--conn-sessions conn))))
               (rexel--attach conn sid pick)
             (funcall pick))))))))


;;; Blocks and layouts of one session

(defun consult-rexel--choice-state (conn sid view)
  "Return a `consult' state previewing the choices of session SID on CONN.
A block is previewed as its buffer and a tab of VIEW as its layout, in
splits of the selected window unless that is dedicated."
  (let ((preview (consult--buffer-preview))
        (blocks (rexel--view-terminal-blocks view))
        (undo nil))
    (lambda (action choice)
      (when undo
        (funcall (prog1 undo (setq undo nil))))
      (funcall preview action
               (pcase choice
                 (`(block . ,bid) (gethash bid (rexel--conn-blocks conn)))))
      (pcase (and (eq action 'preview) choice)
        (`(tab . ,tab)
         (when-let* ((layout (rexel--tab-windows tab))
                     (window (selected-window))
                     ((not (window-minibuffer-p window)))
                     ((not (window-dedicated-p window))))
           (let* ((frame (window-frame window))
                  (before (window-list frame 'no-mini))
                  (buffer (window-buffer window))
                  (start (window-start window))
                  (point (window-point window))
                  (created nil))
             (setq undo
                   (lambda ()
                     (dolist (w created)
                       (when (window-live-p w)
                         (ignore-errors (delete-window w))))
                     (when (and (window-live-p window) (buffer-live-p buffer))
                       (set-window-buffer window buffer)
                       (set-window-start window start t)
                       (set-window-point window point))))
             (unwind-protect
                 (rexel--show-layout conn sid layout blocks window)
               (setq created (seq-difference (window-list frame 'no-mini)
                                             before))))))))))

(defun consult-rexel--read-choice (prompt choices conn sid view)
  "Read one of CHOICES with PROMPT, previewing it; return its CHOICE.
CHOICES, CONN, SID and VIEW are those of `rexel-read-choice-function'."
  (consult--read choices
                 :prompt prompt
                 :require-match t
                 :sort nil
                 :default (caar choices)
                 :lookup #'consult--lookup-cdr
                 :state (consult-rexel--choice-state conn sid view)))

;;;###autoload
(defun consult-rexel-tab ()
  "Pick a tab of a Rex session and rebuild the frame's windows from its layout.
The session is the current block's, else a chosen attached one.  Each
layout is previewed in the selected window."
  (interactive)
  (pcase-let ((`(,conn . ,sid) (rexel--read-session)))
    (rexel--with-view
     conn sid
     (lambda (view)
       (rexel--prompt-later
        (lambda ()
          (let* ((this-command 'consult-rexel-tab)
                 (rexel-read-choice-function #'consult-rexel--read-choice)
                 (tab (rexel--read-tab conn sid view)))
            (rexel--tab-again conn sid (alist-get 'window_id tab)
                              (apply-partially #'rexel--layout-windows
                                               conn sid)))))))))

;;; Embark actions

(defun consult-rexel--read-session (prompt)
  "Read the name of a listed session with PROMPT."
  (completing-read prompt consult-rexel--sessions nil t))

(defun consult-rexel-detach-session (name)
  "Detach from the listed session NAME and kill its block buffers."
  (interactive (list (consult-rexel--read-session "Detach Rex session: ")))
  (pcase-let ((`(,conn . ,sid) (consult-rexel--session name t)))
    (unless (consult-rexel--attached-p (cons conn sid))
      (user-error "Rex session %s is not attached" name))
    (when (rexel--ready-p conn)
      (rexel--request conn "session.detach" `((session_id . ,sid))))
    (rexel--forget-session conn sid)))

(defun consult-rexel--session-request (session method &optional params)
  "Call METHOD with PARAMS for SESSION, a (CONN . SESSION-ID).
A connection redialled since it listed its sessions is a new client, so
`consult-rexel--attach-all' goes first."
  (pcase-let ((`(,conn . ,sid) session))
    (apply #'rexel--request conn
           `(,@consult-rexel--attach-all
             ,(lambda (_result err)
                (if err
                    (message "rexel: session.attach_all: %s"
                             (rexel--error-string err))
                  (rexel--request conn method
                                  `((session_id . ,sid) ,@params))))))))

(defun consult-rexel-rename-session (name label)
  "Label the listed session NAME LABEL.
An empty LABEL restores the label the server chose."
  (interactive
   (let ((name (consult-rexel--read-session "Rename Rex session: ")))
     (list name (read-string (format "New label of %s: " name)))))
  (consult-rexel--session-request (consult-rexel--session name)
                                  "session.set_label" `((label . ,label))))

(defun consult-rexel-kill-session (name)
  "Destroy the listed session NAME on its server, ending its programs."
  (interactive (list (consult-rexel--read-session "Kill Rex session: ")))
  (let ((session (consult-rexel--session name)))
    (when (yes-or-no-p (format "Kill Rex session %s and its programs? " name))
      (consult-rexel--session-request session "session.destroy"))))

(defun consult-rexel--in-block (buffer command)
  "Call COMMAND interactively in BUFFER, which must show a Rex block."
  (unless (consult-rexel--block-p (get-buffer buffer))
    (user-error "%s shows no Rex block" buffer))
  (with-current-buffer buffer
    (call-interactively command)))

(defun consult-rexel-detach-block (buffer)
  "Detach from the session of the Rex block in BUFFER."
  (interactive "bDetach the session of Rex block: ")
  (consult-rexel--in-block buffer #'rexel-detach))

(defun consult-rexel-close-block (buffer)
  "Close the Rex block in BUFFER on the server, ending its program."
  (interactive "bClose Rex block: ")
  (consult-rexel--in-block buffer #'rexel-close-block))

(defun consult-rexel-rename-block-session (buffer)
  "Label the session of the Rex block in BUFFER."
  (interactive "bRename the session of Rex block: ")
  (consult-rexel--in-block buffer #'rexel-rename-session))

(defcustom consult-rexel-embark-key "R"
  "Key in `embark-buffer-map' for the actions on the buffer of a Rex block.
A string for `kbd', bound to `consult-rexel-block-map' when
`consult-rexel-mode' is turned on; nil binds nothing."
  :type '(choice string (const :tag "None" nil))
  :group 'rexel)

(defvar consult-rexel-block-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" #'consult-rexel-detach-block)
    (define-key map "x" #'consult-rexel-close-block)
    (define-key map "$" #'consult-rexel-rename-block-session)
    map)
  "Embark actions on the buffer of a Rex block, keyed as in `rexel-prefix-map'.")

(defvar consult-rexel-session-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'consult-rexel-attach-session)
    (define-key map "a" #'consult-rexel-attach-session)
    (define-key map "d" #'consult-rexel-detach-session)
    (define-key map "$" #'consult-rexel-rename-session)
    (define-key map "k" #'consult-rexel-kill-session)
    map)
  "Embark actions on a session listed by `consult-rexel'.")

(defun consult-rexel--embark (enable)
  "Add the Rex actions to Embark if ENABLE is non-nil, else remove them.
Embark's default action runs the command that opened the minibuffer
again, and `consult-rexel' opens its own later, from a timer; the
overrides name the commands that take the target."
  (when enable
    (set-keymap-parent consult-rexel-session-map embark-general-map))
  (when consult-rexel-embark-key
    (let ((key (kbd consult-rexel-embark-key)))
      (cond (enable (define-key embark-buffer-map key consult-rexel-block-map))
            ((eq (lookup-key embark-buffer-map key) consult-rexel-block-map)
             (define-key embark-buffer-map key nil)))))
  (setf (alist-get 'rexel-session embark-keymap-alist nil t)
        (and enable '(consult-rexel-session-map)))
  (pcase-dolist (`(,key . ,command)
                 '((rexel-session . consult-rexel-attach-session)
                   ((buffer . consult-rexel) . switch-to-buffer)
                   ((buffer . consult-rexel-project) . switch-to-buffer)))
    (setf (alist-get key embark-default-action-overrides nil t #'equal)
          (and enable command))))


;;; Minor mode

;;;###autoload
(define-minor-mode consult-rexel-mode
  "Wire rexel into consult's buffer commands, rexel's prompts and Embark.
The Rex blocks become a hidden source of `consult-ghostel' (narrow key
`r') and `consult-buffer' (`x'), the prompts of `rexel-attach' and
`rexel-sync-layout' preview blocks and layouts, and Embark gets
`consult-rexel-session-map' and, behind `consult-rexel-embark-key',
`consult-rexel-block-map'."
  :global t
  :group 'rexel
  (cond
   (consult-rexel-mode
    (add-to-list 'consult-buffer-sources 'consult-rexel-buffer-source-hidden t)
    (when (boundp 'consult-ghostel-sources)
      (add-to-list 'consult-ghostel-sources 'consult-rexel-source-hidden t))
    (setq rexel-read-choice-function #'consult-rexel--read-choice)
    (when (featurep 'embark)
      (consult-rexel--embark t)))
   (t
    (setq consult-buffer-sources
          (delq 'consult-rexel-buffer-source-hidden consult-buffer-sources))
    (when (boundp 'consult-ghostel-sources)
      (setq consult-ghostel-sources
            (delq 'consult-rexel-source-hidden consult-ghostel-sources)))
    (when (eq rexel-read-choice-function #'consult-rexel--read-choice)
      (setq rexel-read-choice-function #'rexel--read-choice))
    (when (featurep 'embark)
      (consult-rexel--embark nil)))))

(provide 'consult-rexel)
;;; consult-rexel.el ends here
