;;; rexel-setup.el --- Shared elate startup for rexel scenarios -*- lexical-binding: t; -*-

;; Loaded from a scenario's session.eval_file.  Puts rexel from this
;; checkout and its dependencies on the load-path and defines the helpers
;; the scenarios call.  The sandbox's HOME is not the user's, so everything
;; that lives there is found from the real home or from REXEL_ELATE_*
;; environment variables.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst rexel-elate-home
  (file-name-as-directory (expand-file-name (concat "~" (user-real-login-name))))
  "The user's home directory, which the sandbox's HOME hides.")

(defun rexel-elate--dir (env &rest candidates)
  "Return the directory named by the variable ENV, else the first of CANDIDATES.
CANDIDATES are relative to `rexel-elate-home'; the last one is returned
when none exists."
  (or (getenv env)
      (let ((dirs (mapcar (lambda (c) (expand-file-name c rexel-elate-home))
                          candidates)))
        (or (seq-find #'file-directory-p dirs) (car (last dirs))))))

(let* ((here (file-name-directory (or load-file-name buffer-file-name)))
       (repo (expand-file-name "../../../" here))
       (ghostel (rexel-elate--dir "REXEL_ELATE_GHOSTEL"
                                  ".emacs.d/lib/ghostel" ".cache/ghostel"))
       (compat (rexel-elate--dir "REXEL_ELATE_COMPAT" ".emacs.d/lib/compat")))
  (when (file-directory-p compat)
    (add-to-list 'load-path compat))
  (add-to-list 'load-path (rexel-elate--dir "REXEL_ELATE_WEBSOCKET"
                                            ".emacs.d/lib/websocket"
                                            ".cache/emacs-websocket"))
  (add-to-list 'load-path (expand-file-name "lisp" ghostel))
  (add-to-list 'load-path repo)
  (setq ghostel-module-directory ghostel))

(setq load-prefer-newer t
      ring-bell-function 'ignore
      use-short-answers t
      vc-handled-backends nil)

(require 'ghostel)
(require 'rexel)

(defvar rexel-elate-sock
  (or (getenv "REXEL_ELATE_SOCK")
      (if (eq system-type 'darwin)
          (expand-file-name "Library/Application Support/rex/server.sock"
                            rexel-elate-home)
        (format "/run/user/%d/rex/server.sock" (user-real-uid))))
  "Socket of the Rex server the scenarios run against.")

(defvar rexel-elate-label
  (format "%s-%d" (or (getenv "REXEL_ELATE_LABEL") "rexel-elate") (emacs-pid))
  "Label of the session a scenario creates, and prefix of its other ones.")

(defvar rexel-elate-relay-sock
  (format "/tmp/%s.sock" rexel-elate-label)
  "Socket the `unix' relay listens on.")

(defvar rexel-elate-port nil
  "Port the `tcp' relay listens on, chosen when it first starts.")

(defvar rexel-elate-hosts nil
  "The hosts `rexel--hosts' returns in the sandbox.")

;; Host discovery asks the rex program, which looks under HOME.
(advice-add 'rexel--hosts :override (lambda () rexel-elate-hosts))


;;; Hosts and relays

(defun rexel-elate--free-port ()
  "Return a TCP port nothing listens on."
  (let* ((server (make-network-process :name "rexel-elate-port" :server t
                                       :host "127.0.0.1" :service t
                                       :family 'ipv4 :noquery t))
         (port (process-contact server :service)))
    (delete-process server)
    port))

(defun rexel-elate--listen (kind)
  "Return the socat address the relay of KIND, `unix' or `tcp', listens on."
  (pcase kind
    ('unix (format "UNIX-LISTEN:%s,fork,backlog=64" rexel-elate-relay-sock))
    ('tcp (format "TCP-LISTEN:%d,bind=127.0.0.1,reuseaddr,fork,backlog=64"
                  (or rexel-elate-port
                      (setq rexel-elate-port (rexel-elate--free-port)))))))

(defvar rexel-elate--relays nil
  "Alist of relay kind and the socat process listening for it.")

(defun rexel-elate--relay-name (kind)
  "Return the name the relay of KIND runs under.
It is on the command line of the relay and of the children carrying its
connections, for pgrep and pkill."
  (format "%s-%s" rexel-elate-label kind))

(defun rexel-elate-relay-running-p (kind)
  "Return non-nil while the relay of KIND accepts connections."
  (process-live-p (alist-get kind rexel-elate--relays)))

(defun rexel-elate-relay-start (kind)
  "Start the socat relay of KIND to the server and wait until it listens.
Return its process."
  (unless (rexel-elate-relay-running-p kind)
    (when (eq kind 'unix)
      (ignore-errors (delete-file rexel-elate-relay-sock)))
    (let ((process (make-process
                    :name (format "rexel-elate-relay-%s" kind)
                    :command (list "socat" "-lp" (rexel-elate--relay-name kind)
                                   (rexel-elate--listen kind)
                                   (format "UNIX-CONNECT:\"%s\"" rexel-elate-sock))
                    :connection-type 'pipe :noquery t)))
      (with-timeout (5 (error "The %s relay does not listen" kind))
        (while (not (if (eq kind 'unix)
                        (rexel--socket-live-p rexel-elate-relay-sock)
                      (ignore-errors
                        (delete-process
                         (make-network-process :name "rexel-elate-probe"
                                               :host "127.0.0.1"
                                               :service rexel-elate-port
                                               :noquery t))
                        t)))
          (sleep-for 0.05)))
      (setf (alist-get kind rexel-elate--relays) process))))

(defun rexel-elate-relay-stop (kind)
  "Kill the relay of KIND with every connection it carries."
  (call-process "pkill" nil nil nil "-KILL" "-f"
                (format "socat -lp %s " (rexel-elate--relay-name kind)))
  (when-let* ((process (alist-get kind rexel-elate--relays)))
    (with-timeout (5)
      (while (process-live-p process)
        (accept-process-output process 0.05))))
  (when (eq kind 'unix)
    (ignore-errors (delete-file rexel-elate-relay-sock))))

(defvar rexel-elate--frozen nil
  "Pids of the relay children `rexel-elate-relay-freeze' stopped.")

(defun rexel-elate-relay-freeze (kind)
  "Stop the relay of KIND from carrying its open connections.
The listener keeps accepting new ones.  `rexel-elate-relay-reap' ends
the stopped ones."
  (let ((listener (process-id (alist-get kind rexel-elate--relays))))
    (setq rexel-elate--frozen
          (split-string (shell-command-to-string (format "pgrep -P %d" listener))))
    (dolist (pid rexel-elate--frozen)
      (call-process "kill" nil nil nil "-STOP" pid))
    rexel-elate--frozen))

(defun rexel-elate-relay-reap ()
  "Kill the connections `rexel-elate-relay-freeze' stopped."
  (dolist (pid rexel-elate--frozen)
    (call-process "kill" nil nil nil "-KILL" pid))
  (setq rexel-elate--frozen nil))

(defun rexel-elate-host (kind)
  "Return the (LABEL . ENDPOINTS) host of KIND, starting the relay it needs.
`local' is this machine's server, `unix' and `tcp' reach it through a
relay and carry a label, as a server on another machine does.  `tls' is
the server's tailnet address in REXEL_ELATE_TAILNET.  `fallback' lists a
dead endpoint before the tcp relay."
  (pcase kind
    ('local (list nil rexel-elate-sock))
    ('unix (rexel-elate-relay-start 'unix)
           (list "unix" rexel-elate-relay-sock))
    ('tcp (rexel-elate-relay-start 'tcp)
          (list "tcp" (format "http://127.0.0.1:%d" rexel-elate-port)))
    ('fallback (rexel-elate-relay-start 'tcp)
               (cons "fallback"
                     (rexel--endpoints
                      (list (format "http://127.0.0.1:%d" (rexel-elate--free-port))
                            (format "http://127.0.0.1:%d" rexel-elate-port)))))
    ('tls (cons "tls" (rexel--endpoints
                       (list (or (getenv "REXEL_ELATE_TAILNET")
                                 (error "REXEL_ELATE_TAILNET is not set"))))))))

(defun rexel-elate-use (&rest kinds)
  "Make the hosts of KINDS the ones rexel knows, and return them."
  (setq rexel-elate-hosts
        (mapcar #'rexel-elate-host
                (mapcar (lambda (k) (if (stringp k) (intern k) k)) kinds))))


;;; Sessions and block buffers

(defun rexel-elate-blocks (&optional label)
  "Return the live block buffers, those of the session labelled LABEL if given.
The selected window's buffer comes first when it is one of them."
  (let ((buffers (seq-filter
                  (lambda (buf)
                    (and (buffer-local-value 'rexel--block-id buf)
                         (or (null label)
                             (string-prefix-p (format "*rex:%s" label)
                                              (buffer-name buf)))))
                  (buffer-list)))
        (selected (window-buffer)))
    (if (memq selected buffers)
        (cons selected (remq selected buffers))
      buffers)))

(defun rexel-elate-block ()
  "Return the block buffer to act on: the selected window's, else the first."
  (or (car (rexel-elate-blocks)) (error "No block buffer")))

(defun rexel-elate-live-p (&optional buffer)
  "Return non-nil when BUFFER's block streams live; default `rexel-elate-block'."
  (when-let* ((buf (or buffer (car (rexel-elate-blocks)))))
    (eq (buffer-local-value 'rexel--data-state buf) 'live)))

(defun rexel-elate-new-session (&optional suffix)
  "Create and attach a session labelled `rexel-elate-label' plus SUFFIX.
It is made on the first host in use."
  (let ((host (car rexel-elate-hosts))
        (label (concat rexel-elate-label suffix)))
    (cl-letf (((symbol-function 'rexel--read-host) (lambda () host)))
      (rexel-new-session label))
    label))

(defun rexel-elate-start (kind)
  "Use the host of KIND alone and attach a new session on it."
  (rexel-elate-use kind)
  (rexel-elate-new-session))

(defun rexel-elate-shell ()
  "Replace the block's shell with a bash that reads no startup files.
Its prompt is \"$ \"."
  (rexel-elate-send "exec /bin/bash --norc --noprofile\n")
  (rexel-elate-send "PS1='$ '\n"))

(defun rexel-elate-prompt-p (&optional buffer)
  "Return non-nil when the terminal of BUFFER ends in the \"$ \" prompt."
  (and (rexel-elate-live-p buffer)
       (string-match-p "\\(?:\\`\\|\n\\)\\$\\'"
                       (string-trim-right (rexel-elate-text buffer)))
       t))

(defun rexel-elate-answer (string &optional keep)
  "Put STRING into the open minibuffer, replacing its contents unless KEEP."
  (with-selected-window (or (active-minibuffer-window)
                            (error "No minibuffer is open"))
    (unless keep
      (delete-minibuffer-contents))
    (insert string)
    string))

(defun rexel-elate-choices ()
  "Return the completions the open minibuffer offers, in their order."
  (with-selected-window (or (active-minibuffer-window)
                            (error "No minibuffer is open"))
    (all-completions "" minibuffer-completion-table)))

(defun rexel-elate-answer-label (&optional suffix)
  "Put `rexel-elate-label' plus SUFFIX into the open minibuffer."
  (rexel-elate-answer (concat rexel-elate-label suffix)))

(defun rexel-elate-buffer-name-p (&optional suffix buffer)
  "Return non-nil when BUFFER is named after the session with SUFFIX.
The name is *rex:LABEL:TAB* and *rex:LABEL@HOST:TAB* on a labelled host."
  (let* ((buf (or buffer (rexel-elate-block)))
         (conn (buffer-local-value 'rexel--conn buf))
         (host (and conn (rexel--conn-host conn))))
    (and (string-match-p
          (format "\\`\\*rex:%s%s:[^:*]+\\*\\'"
                  (regexp-quote (concat rexel-elate-label suffix))
                  (if host (concat "@" (regexp-quote host)) ""))
          (buffer-name buf))
         t)))

(defun rexel-elate-named-p (tab &optional buffer)
  "Return non-nil when BUFFER is named after the scenario's session and TAB."
  (let* ((buf (or buffer (rexel-elate-block)))
         (conn (buffer-local-value 'rexel--conn buf))
         (host (and conn (rexel--conn-host conn))))
    (equal (buffer-name buf)
           (format "*rex:%s%s:%s*" rexel-elate-label
                   (if host (concat "@" host) "") tab))))

(defun rexel-elate-call (method &optional payload conn)
  "Call the control METHOD with PAYLOAD on CONN and return its result.
CONN defaults to the block buffer's connection.  Signal the server's error."
  (let* ((conn (or conn (buffer-local-value 'rexel--conn (rexel-elate-block))))
         (answer nil))
    (rexel--request conn method payload
                    (lambda (result err) (setq answer (list result err))))
    (with-timeout (10 (error "No answer to %s" method))
      (while (null answer)
        (accept-process-output nil 0.05)))
    (when (cadr answer)
      (error "%s: %s" method (rexel--error-string (cadr answer))))
    (car answer)))

(defun rexel-elate-sessions (&optional conn)
  "Return the (LABEL . ID) of every session on the server of CONN."
  (mapcar (lambda (s) (cons (alist-get 'label s) (alist-get 'session_id s)))
          (alist-get 'sessions (rexel-elate-call "session.list" nil conn))))

(defun rexel-elate-session-id (label &optional conn)
  "Return the ID of the session labelled LABEL on the server of CONN, or nil."
  (cdr (assoc label (rexel-elate-sessions conn))))

(defun rexel-elate-client-count (sid)
  "Return the number of rexel clients connected and attached to SID.
A control connection of its own attaches to ask, and is not counted.
Clients whose connection was lost stay listed for a while and are skipped."
  (rexel-elate--control
   (lambda (conn)
     (rexel-elate-call "session.attach" `((session_id . ,sid)) conn)
     (1- (seq-count
          (lambda (client)
            (and (equal (alist-get 'name (alist-get 'info client)) "rexel")
                 (equal (alist-get 'connection_state client) "connected")))
          (alist-get 'clients
                     (rexel-elate-call "session.list_clients"
                                       `((session_id . ,sid)) conn)))))))

(defun rexel-elate-view ()
  "Return the server's view of the block buffer's session."
  (with-current-buffer (rexel-elate-block)
    (rexel-elate-call "session.view" `((session_id . ,rexel--session-id)))))

(defun rexel-elate-tabs ()
  "Return, for each tab of the block buffer's session, its number of blocks."
  (mapcar (lambda (window)
            (length (alist-get 'blocks (seq-first (alist-get 'layers window)))))
          (alist-get 'windows (rexel-elate-view))))

(defun rexel-elate--control (fn)
  "Call FN with a ready control connection of its own to the server."
  (let* ((rexel--conns nil)
         (done nil)
         (conn nil))
    (rexel--connect (list nil rexel-elate-sock)
                    (lambda (c) (setq conn c done t)))
    (with-timeout (10 (error "Cannot reach %s" rexel-elate-sock))
      (while (not done) (accept-process-output nil 0.05)))
    (unwind-protect
        (funcall fn conn)
      (ignore-errors (websocket-close (rexel--conn-ws conn))))))

(defun rexel-elate-on-server-p (&optional suffix)
  "Return non-nil when the server has the session with SUFFIX."
  (and (rexel-elate--control
        (lambda (conn)
          (rexel-elate-session-id (concat rexel-elate-label suffix) conn)))
       t))

(defun rexel-elate-delay-request (method seconds)
  "Send every later request for METHOD only after SECONDS, as a slow link would.
`rexel-elate-undelay-requests' ends that."
  (advice-add 'rexel--request :around
              (lambda (request conn name &rest args)
                (if (equal name method)
                    (run-at-time seconds nil
                                 (lambda () (apply request conn name args)))
                  (apply request conn name args)))
              '((name . rexel-elate-delay))))

(defun rexel-elate-undelay-requests ()
  "Send requests at once again."
  (advice-remove 'rexel--request 'rexel-elate-delay))

(defun rexel-elate--destroy (conn sid)
  "Destroy session SID on CONN, which has to attach to it first."
  (rexel-elate-call "session.attach" `((session_id . ,sid)) conn)
  (rexel-elate-call "session.destroy" `((session_id . ,sid)) conn))

(defun rexel-elate-destroy (&optional suffix)
  "Destroy the session labelled `rexel-elate-label' plus SUFFIX from outside.
Another client does it, on a connection the attached one does not share."
  (rexel-elate--control
   (lambda (conn)
     (when-let* ((sid (rexel-elate-session-id (concat rexel-elate-label suffix)
                                              conn)))
       (rexel-elate--destroy conn sid)))))

(defvar rexel-elate--other nil
  "The connection of the client `rexel-elate-other-take' plays.")

(defun rexel-elate-other-take (cols rows &optional buffer)
  "As another client, take the block of BUFFER and size it COLS by ROWS.
That client stays connected until `rexel-elate-other-leave'.  BUFFER
defaults to `rexel-elate-block'."
  (let* ((buf (or buffer (rexel-elate-block)))
         (sid (buffer-local-value 'rexel--session-id buf))
         (bid (buffer-local-value 'rexel--block-id buf)))
    (unless rexel-elate--other
      (let ((rexel--conns nil))
        (rexel--connect (list nil rexel-elate-sock)
                        (lambda (c) (setq rexel-elate--other c))))
      (with-timeout (10 (error "Cannot reach %s" rexel-elate-sock))
        (while (not rexel-elate--other) (accept-process-output nil 0.05))))
    (rexel-elate-call "session.attach" `((session_id . ,sid)) rexel-elate--other)
    (rexel-elate-call "session.own_block"
                      `((session_id . ,sid) (block_ids . ,(vector bid))
                        (mode . "take"))
                      rexel-elate--other)
    (rexel-elate-call "com.superlogical.terminal.resize"
                      `((session_id . ,sid) (block_id . ,bid)
                        (args . ((columns . ,cols) (rows . ,rows))))
                      rexel-elate--other)
    t))

(defun rexel-elate-other-leave ()
  "Disconnect the client of `rexel-elate-other-take'."
  (when rexel-elate--other
    (ignore-errors (websocket-close (rexel--conn-ws rexel-elate--other)))
    (setq rexel-elate--other nil))
  t)

(defun rexel-elate-mode-line (&optional buffer)
  "Return the text of the mode line of the window showing BUFFER.
BUFFER defaults to `rexel-elate-block'."
  (let ((buf (or buffer (rexel-elate-block))))
    (format-mode-line (buffer-local-value 'mode-line-format buf) nil
                      (get-buffer-window buf) buf)))

(defun rexel-elate-preview ()
  "Show the block in the selected window while reading a string, as a picker does.
The window shows its own buffer again afterwards."
  (interactive)
  (let* ((window (selected-window))
         (shown (window-buffer window))
         (block (rexel-elate-block)))
    (minibuffer-with-setup-hook (lambda () (set-window-buffer window block))
      (unwind-protect
          (read-string "Preview: ")
        (set-window-buffer window shown)))))

(defvar rexel-elate-files-dir)

(defun rexel-elate-cleanup ()
  "Destroy every session a scenario created and stop the relays."
  (ignore-errors
    (rexel-elate--control
     (lambda (conn)
       (pcase-dolist (`(,label . ,sid) (rexel-elate-sessions conn))
         (when (string-prefix-p rexel-elate-label label)
           (ignore-errors (rexel-elate--destroy conn sid)))))))
  (rexel-elate-relay-stop 'unix)
  (when rexel-elate-port
    (rexel-elate-relay-stop 'tcp))
  (ignore-errors (delete-directory rexel-elate-files-dir t))
  t)

(add-hook 'kill-emacs-hook #'rexel-elate-cleanup)


;;; The terminal

(defun rexel-elate-text (&optional buffer)
  "Return the whole terminal text of BUFFER, by default `rexel-elate-block'.
It is read from the terminal, so the buffer need not be displayed."
  (with-current-buffer (or buffer (rexel-elate-block))
    (ghostel--copy-all-text ghostel--term)))

(defun rexel-elate-has (regexp &optional buffer)
  "Return non-nil when REGEXP matches the terminal text of BUFFER."
  (and (string-match-p regexp (rexel-elate-text buffer)) t))

(defun rexel-elate-shows (regexp &optional buffer)
  "Return non-nil when REGEXP matches the Emacs buffer BUFFER.
Unlike `rexel-elate-has' this sees only what ghostel rendered into it.
BUFFER defaults to `rexel-elate-block'."
  (with-current-buffer (or buffer (rexel-elate-block))
    (save-excursion
      (goto-char (point-min))
      (and (re-search-forward regexp nil t) t))))

(defun rexel-elate-count (regexp &optional buffer)
  "Return how often REGEXP matches in the terminal text of BUFFER."
  (let ((text (rexel-elate-text buffer))
        (start 0)
        (count 0))
    (while (string-match regexp text start)
      (setq count (1+ count)
            start (max (1+ start) (match-end 0))))
    count))

(defun rexel-elate-send (string &optional buffer)
  "Type STRING into the block of BUFFER as terminal input."
  (with-current-buffer (or buffer (rexel-elate-block))
    (ghostel-send-string string)))

(defun rexel-elate-size (&optional buffer)
  "Return the (COLS . ROWS) of the terminal in BUFFER."
  (with-current-buffer (or buffer (rexel-elate-block))
    (cons ghostel--term-cols ghostel--term-rows)))

(defun rexel-elate-size-line (tag &optional buffer)
  "Return the regexp of the line \"TAG ROWS COLS\" for BUFFER's terminal size.
`echo TAG $(stty size)' prints that line when the program agrees."
  (pcase-let ((`(,cols . ,rows) (rexel-elate-size buffer)))
    (format "^%s %d %d$" tag rows cols)))

(defun rexel-elate-window-size (&optional buffer)
  "Return the (COLS . ROWS) the windows showing BUFFER give its terminal.
A window gives the columns a line fills without wrapping and its whole
text lines, as for a process in Emacs; of several windows the smallest
of each wins.  BUFFER defaults to `rexel-elate-block'.  Nil when no
window shows it."
  (when-let* ((windows (get-buffer-window-list
                        (or buffer (rexel-elate-block)) nil t)))
    (cons (apply #'min (mapcar #'window-max-chars-per-line windows))
          (apply #'min (mapcar (lambda (w)
                                 (with-selected-window w
                                   (floor (window-screen-lines))))
                               windows)))))

(defun rexel-elate-fits-p (&optional buffer)
  "Return non-nil when BUFFER's terminal has the size its windows give it."
  (equal (rexel-elate-size buffer) (rexel-elate-window-size buffer)))

(defun rexel-elate-drop-connection (which)
  "Drop a connection of the block buffer as a dead network would.
WHICH is `data' for its stream or `control' for its server connection."
  (with-current-buffer (rexel-elate-block)
    (delete-process
     (websocket-conn (pcase which
                       ('data (rexel--stream-ws rexel--stream))
                       ('control (rexel--conn-ws rexel--conn)))))))

(defun rexel-elate-drop-after-connect ()
  "Drop the control connection right after the next `data.connect' is sent.
Its answer is lost with the connection."
  (advice-add 'rexel--request :around
              (lambda (request conn name &rest args)
                (apply request conn name args)
                (when (equal name "data.connect")
                  (advice-remove 'rexel--request 'rexel-elate-drop)
                  (delete-process (websocket-conn (rexel--conn-ws conn)))))
              '((name . rexel-elate-drop))))

(defun rexel-elate-spoil-hello ()
  "Make the next HELLO carry a ticket the server does not know."
  (advice-add 'rexel--hello :filter-return
              (lambda (_hello)
                (advice-remove 'rexel--hello 'rexel-elate-spoil)
                (concat "\x01" (make-string 16 ?x)))
              '((name . rexel-elate-spoil))))

(defun rexel-elate-in-order-p (&rest regexps)
  "Return non-nil when REGEXPS match the terminal text one after another."
  (let ((text (rexel-elate-text))
        (start 0))
    (while (and regexps start)
      (setq start (and (string-match (pop regexps) text start) (match-end 0))))
    (and start t)))

(defun rexel-elate-clear-messages ()
  "Empty the *Messages* buffer, so that `rexel-elate-messages-p' sees new ones."
  (with-current-buffer (messages-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer))))

(defvar rexel-elate--ticks nil
  "Times at which the timer of `rexel-elate-ticker-start' ran, newest first.")

(defvar rexel-elate--ticker nil
  "The timer of `rexel-elate-ticker-start'.")

(defun rexel-elate-ticker-start ()
  "Start a timer that notes the time every 50 ms."
  (setq rexel-elate--ticks (list (float-time))
        rexel-elate--ticker
        (run-at-time 0.05 0.05 (lambda () (push (float-time) rexel-elate--ticks)))))

(defun rexel-elate-ticker-stop ()
  "Stop the ticker and return the longest time between two of its runs."
  (cancel-timer rexel-elate--ticker)
  (let ((ticks (cons (float-time) rexel-elate--ticks))
        (longest 0))
    (while (cdr ticks)
      (setq longest (max longest (- (car ticks) (cadr ticks)))
            ticks (cdr ticks)))
    longest))

(defun rexel-elate-messages-p (regexp)
  "Return non-nil when REGEXP matches in the *Messages* buffer."
  (with-current-buffer (messages-buffer)
    (save-excursion
      (goto-char (point-min))
      (and (re-search-forward regexp nil t) t))))


;;; Files to upload

(defvar rexel-elate-files-dir
  (expand-file-name (format "rexel-elate-files-%d" (emacs-pid))
                    temporary-file-directory)
  "Directory of the files a scenario creates.")

(defun rexel-elate-file (name bytes)
  "Create the file NAME of BYTES random-looking bytes and return its path."
  (let ((file (expand-file-name name rexel-elate-files-dir))
        (coding-system-for-write 'binary))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (set-buffer-multibyte nil)
      (let ((line (concat (secure-hash 'sha256 name) "\n")))
        (dotimes (_ (/ bytes (length line)))
          (insert line))
        (insert (substring line 0 (% bytes (length line))))))
    file))

(defun rexel-elate-sparse-file (name bytes)
  "Create the file NAME holding BYTES zero bytes and return its path."
  (let ((file (expand-file-name name rexel-elate-files-dir)))
    (make-directory (file-name-directory file) t)
    (call-process "dd" nil nil nil "if=/dev/zero" (concat "of=" file)
                  "bs=1" "count=0" (format "seek=%d" bytes))
    file))

(defun rexel-elate-files (count bytes)
  "Create COUNT files of BYTES bytes, named many-NN.txt, and return their paths."
  (mapcar (lambda (i) (rexel-elate-file (format "many-%02d.txt" i) bytes))
          (number-sequence 1 count)))

(defconst rexel-elate-png
  (base64-decode-string
   (concat "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhf"
           "DwAChwGA60e6kgAAAABJRU5ErkJggg=="))
  "A PNG image of one pixel.")

(defun rexel-elate-gz-file (name bytes)
  "Create the file NAME of BYTES bytes, gzip it and return the path of NAME.gz."
  (let ((file (rexel-elate-file name bytes)))
    (call-process "gzip" nil nil nil "-f" file)
    (concat file ".gz")))

(defun rexel-elate-png-line ()
  "Return the regexp of the line `cksum' prints for a pasted `rexel-elate-png'."
  (let ((file (expand-file-name "pasted.png" rexel-elate-files-dir))
        (coding-system-for-write 'binary))
    (make-directory rexel-elate-files-dir t)
    (write-region rexel-elate-png nil file nil 'silent)
    (format "^%s .*ghostel-clipboard-[^/]*\\.png$" (rexel-elate-cksum file))))

(defun rexel-elate-tramp-file (file)
  "Return the name of the local FILE as a TRAMP host sees it.
The `mock' method is a shell on this machine, as in TRAMP's own tests."
  (require 'tramp)
  (add-to-list 'tramp-methods
               `("mock"
                 (tramp-login-program ,tramp-default-remote-shell)
                 (tramp-login-args (("-i")))
                 (tramp-remote-shell ,tramp-default-remote-shell)
                 (tramp-remote-shell-args ("-c"))
                 (tramp-connection-timeout 10)))
  (concat "/mock::" file))

(defun rexel-elate-cksum-line (file)
  "Return the regexp of the line `cksum' prints for a copy of FILE."
  (format "^%s .*%s$" (rexel-elate-cksum file)
          (regexp-quote (file-name-nondirectory file))))

(defun rexel-elate-dnd (files)
  "Drop FILES on the block buffer's window as the X11 and pgtk ports do."
  (with-current-buffer (rexel-elate-block)
    (dnd-handle-multiple-urls
     (get-buffer-window) (mapcar (lambda (f) (concat "file://" f)) files) 'copy)))

(defun rexel-elate-drop (files)
  "Drop FILES on the block buffer's window as the NS and w32 ports do.
The event goes through the command loop to the buffer's binding."
  (with-current-buffer (rexel-elate-block)
    (push `(drag-n-drop ,(posn-at-point (window-point (get-buffer-window))
                                        (get-buffer-window))
                        (file (copy) ,@files))
          unread-command-events)))

(defun rexel-elate-yank-media (mimetype data)
  "Paste clipboard DATA of MIMETYPE with the handler `yank-media' would call."
  (with-current-buffer (rexel-elate-block)
    (funcall (cdr (seq-find (lambda (h)
                              (if (stringp (car h))
                                  (string-match-p (car h) (symbol-name mimetype))
                                (eq (car h) mimetype)))
                            yank-media--registered-handlers))
             mimetype data)))

(defun rexel-elate-cksum (file)
  "Return the checksum and size `cksum' prints for FILE, as \"SUM SIZE\"."
  (with-temp-buffer
    (call-process "cksum" nil t nil file)
    (goto-char (point-min))
    (looking-at "\\([0-9]+ [0-9]+\\)")
    (match-string 1)))

(defun rexel-elate-uploads ()
  "Return the files the server stores for the block buffer's block.
The server runs on this machine and keeps them beside its socket."
  (with-current-buffer (rexel-elate-block)
    (seq-mapcat
     (lambda (dir) (directory-files-recursively dir ""))
     ;; uploads/SERVER-PID/BLOCK/UPLOAD/NAME
     (file-expand-wildcards
      (expand-file-name (concat "uploads/*/"
                                (car (last (split-string rexel--block-id ":"))))
                        (file-name-directory rexel-elate-sock))))))

(defun rexel-elate-uploading-p (&optional buffer)
  "Return non-nil while BUFFER's block has files in a batch or queued."
  (with-current-buffer (or buffer (rexel-elate-block))
    (and (or rexel--uploads rexel--upload-queue) t)))

(defvar rexel-elate--hold-at nil
  "Bytes of its file after which `rexel-elate-hold-upload' stops an upload.")

(defvar rexel-elate--held nil
  "The arguments of the `rexel--upload-send-chunk' call held back, if any.")

(defun rexel-elate--hold (send buf upload)
  "Call SEND with BUF and UPLOAD unless the hold is due; then keep them."
  (if (and rexel-elate--hold-at
           (>= (rexel--upload-sent upload) rexel-elate--hold-at))
      (setq rexel-elate--held (list buf upload)
            rexel-elate--hold-at nil)
    (funcall send buf upload)))

(advice-add 'rexel--upload-send-chunk :around #'rexel-elate--hold)

(defun rexel-elate-hold-upload (bytes)
  "Stop the next upload that has sent BYTES of its file, as a slow link would.
Its connection stays open; `rexel-elate-release-upload' sends the rest.
A fast link sends a file before a step can look at it."
  (setq rexel-elate--hold-at bytes
        rexel-elate--held nil))

(defun rexel-elate-upload-held-p ()
  "Return non-nil while `rexel-elate-hold-upload' holds an upload."
  (and rexel-elate--held t))

(defun rexel-elate-release-upload ()
  "Send the rest of the held upload, from a timer as rexel does."
  (when-let* ((args rexel-elate--held))
    (setq rexel-elate--held nil)
    (apply #'run-at-time 0 nil #'rexel--upload-send-chunk args)
    t))

(defvar rexel-elate--key-sent nil
  "Time `rexel-elate-key-probe-arm' ran.")

(defvar rexel-elate--key-at nil
  "Time the command `rexel-elate-key-probe' ran.")

(defvar rexel-elate-key-uploading nil
  "Non-nil if a block buffer was uploading when `rexel-elate-key-probe' ran.")

(defun rexel-elate-key-probe ()
  "Note when this command runs; type M-x rexel-elate-key-probe RET raw."
  (interactive)
  (setq rexel-elate--key-at (float-time)
        rexel-elate-key-uploading
        (and (seq-some #'rexel-elate-uploading-p (rexel-elate-blocks)) t)))

(defun rexel-elate-key-probe-arm ()
  "Forget the last run of `rexel-elate-key-probe' and note the time."
  (setq rexel-elate--key-at nil
        rexel-elate--key-sent (float-time)))

(defun rexel-elate-key-latency ()
  "Return the seconds from `rexel-elate-key-probe-arm' to the probe, or nil."
  (and rexel-elate--key-at (- rexel-elate--key-at rexel-elate--key-sent)))

(defvar rexel-elate--progress nil
  "The bytes the block buffer's batch had sent, and since when.")

(defun rexel-elate-upload-progress ()
  "Return the bytes the block buffer's batch has sent so far, nil without one."
  (with-current-buffer (rexel-elate-block)
    (and rexel--uploads (apply #'+ (mapcar #'rexel--upload-sent rexel--uploads)))))

(defun rexel-elate-upload-stalled-p (seconds)
  "Return non-nil once the block buffer's batch has sent nothing for SECONDS.
Each call notes the progress; the count starts at the first call that
sees it unchanged."
  (let ((sent (rexel-elate-upload-progress)))
    (if (and sent (eql sent (car rexel-elate--progress)))
        (>= (- (float-time) (cdr rexel-elate--progress)) seconds)
      (setq rexel-elate--progress (cons sent (float-time)))
      nil)))

(defvar rexel-elate--output-reads nil
  "Times Emacs read output of `rexel-elate-output-ticker-start', newest first.")

(defvar rexel-elate--output-ticker nil
  "The process of `rexel-elate-output-ticker-start'.")

(defun rexel-elate-output-ticker-start ()
  "Start a process that prints a line every 100 ms and note each read."
  (setq rexel-elate--output-reads (list (float-time))
        rexel-elate--output-ticker
        (make-process :name "rexel-elate-output-ticker" :noquery t
                      :command '("sh" "-c" "while :; do echo x; sleep 0.1; done")
                      :filter (lambda (_process _output)
                                (push (float-time) rexel-elate--output-reads)))))

(defun rexel-elate-output-ticker-stop ()
  "Stop the output ticker and return the longest time between two reads."
  (delete-process rexel-elate--output-ticker)
  (let ((reads (cons (float-time) rexel-elate--output-reads))
        (longest 0))
    (while (cdr reads)
      (setq longest (max longest (- (car reads) (cadr reads)))
            reads (cdr reads)))
    longest))

(provide 'rexel-setup)
;;; rexel-setup.el ends here
