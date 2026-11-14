;;; rexel-transfer.el --- File uploads and downloads for rexel -*- lexical-binding: t; -*-

;; Author: Daniel Kraus <daniel@kraus.my>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Files copied between this machine and the one a Rex block runs on.
;; A file dropped on a block buffer, pasted with `yank-media' or given to
;; `rexel-upload-file' goes to the block's machine, where the server
;; types its path; `rexel-download-file' copies a file from there.  Each
;; file has a data connection of its own, beside the block's stream.
;; rexel.el loads this file.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ghostel)
(require 'websocket)

(defvar rexel--conn)
(defvar rexel--session-id)
(defvar rexel--block-id)
(defvar rexel--writes)
(defvar rexel--writes-overdue)

(declare-function rexel--conn-ws "rexel" (cl-x) t)
(declare-function rexel--conn-host "rexel" (cl-x) t)
(declare-function rexel--conn-attaching "rexel" (cl-x) t)
(declare-function rexel--conn-name "rexel" (conn))
(declare-function rexel--ready-p "rexel" (conn))
(declare-function rexel--request "rexel" (conn method payload &optional callback))
(declare-function rexel--error-string "rexel" (err))
(declare-function rexel--ws-filter "rexel" (process output))
(declare-function rexel--write-p "rexel" (cl-x) t)
(declare-function rexel--watched-write "rexel" (process on-stall write &optional size))
(declare-function rexel--open-data-ws "rexel" (protocol hello on-message on-close))
(declare-function rexel--hello "rexel" (ticket))
(declare-function rexel--handshake "rexel" (bytes))
(declare-function rexel--prompt-later "rexel" (function))
(declare-function rexel--ensure-block-buffer-context "rexel" ())
(declare-function dired-get-marked-files "dired")


;;; Uploads

(defvar-local rexel--upload-queue nil
  "Local files waiting to be uploaded to this block, in order.")

(defvar-local rexel--uploads nil
  "The `rexel--upload's of the batch in progress, in the order of its paths.")

(defconst rexel--upload-chunk-bytes 4096
  "Bytes of a file sent in one message of an upload.
The server takes up to 65536.  A message that fits the send buffer of a
Unix socket, 8192 bytes on macOS, is written at once; for the rest of a
larger one Emacs looks for room only every 20 ms.")

(defconst rexel--upload-batch-max 16
  "Most uploads a terminal takes at a time.")

(defvar-local rexel--upload-reporter nil
  "Progress reporter of the batch in progress.")

(defvar rexel--upload-slice 0.05
  "Seconds an upload sends for before Emacs reads what other processes sent.")

(cl-defstruct (rexel--upload (:constructor rexel--make-upload) (:copier nil))
  "One file on its way to the machine a block runs on.
FILE is the local file, NAME the name it gets, SIZE its bytes, nil until
`rexel--upload-fetch', and SENT those sent so far.  REF names its data
stream and WS is that stream's WebSocket.  STATE is `begin' while the
ticket is requested, `welcome' until the server answers the HELLO, then
`ready', `sending', `ending' once the end marker is sent, and `done'.
TEMP is non-nil when FILE is a copy to delete afterwards."
  file name size ref ws state (sent 0) temp)

(defun rexel--uuid ()
  "Return a random version 4 UUID in lower case."
  (format "%08x-%04x-4%03x-%04x-%012x"
          (random (ash 1 32)) (random (ash 1 16)) (random (ash 1 12))
          (logior #x8000 (random (ash 1 14))) (random (ash 1 48))))

(defun rexel--send-files (files)
  "Hand FILES to the program in this block by typing their paths.
When the server runs on this machine and all FILES are local, their own
paths are typed.  Otherwise the files are uploaded in batches of
`rexel--upload-batch-max' and the server types the paths it stores them
under; calls made in one command share a batch."
  (cond
   ((null rexel--conn)
    (user-error "Not connected; use M-x rexel-attach"))
   ((and (null (rexel--conn-host rexel--conn))
         (not (seq-some #'file-remote-p files)))
    (ghostel-paste-string
     (concat (mapconcat #'shell-quote-argument files " ") " ")))
   (t
    (dolist (file files)
      (unless (and (file-regular-p file) (file-readable-p file))
        (user-error "Cannot upload %s: not a readable file" file)))
    (setq rexel--upload-queue (append rexel--upload-queue files))
    (run-at-time 0 nil #'rexel--upload-next (current-buffer)))))

(defun rexel--upload-next (buf)
  "Start the next batch of BUF's queued files once it can go out.
That is once no write is innermost and BUF's session is attached.  A
fetch of a remote file inside a write's wait would keep it from
returning, until it counts as stalled."
  (if (or (rexel--write-p (car rexel--writes))
          (and (buffer-live-p buf)
               (with-current-buffer buf
                 (and (rexel--ready-p rexel--conn)
                      (member rexel--session-id
                              (rexel--conn-attaching rexel--conn))))))
      (run-at-time 0.01 nil #'rexel--upload-next buf)
    (rexel--upload-start buf)))

(defun rexel--upload-start (buf)
  "Start the next batch of BUF's queued files unless one is in progress.
Remote files are fetched, then every file is announced before any is
sent, so the server refuses a batch over its limits with nothing sent."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (and rexel--upload-queue (null rexel--uploads))
        (let* ((files (seq-take rexel--upload-queue rexel--upload-batch-max))
               (count (length files))
               (batch (and (> count 1) (rexel--uuid)))
               (uploads (mapcar (lambda (file)
                                  (rexel--make-upload
                                   :file file :state 'begin
                                   :name (file-name-nondirectory file)
                                   :ref (concat "upload:" (rexel--uuid))))
                                files)))
          (setq rexel--upload-queue (nthcdr count rexel--upload-queue)
                rexel--uploads uploads)
          (condition-case err
              (progn
                (unless (rexel--ready-p rexel--conn)
                  (error "Not connected"))
                (when (seq-every-p (lambda (upload)
                                     (rexel--upload-fetch buf upload))
                                   uploads)
                  (setq rexel--upload-reporter
                        (make-progress-reporter
                         (format "Uploading %d file%s to %s..."
                                 count (if (= count 1) "" "s")
                                 (rexel--conn-name rexel--conn))
                         0 (max 1 (apply #'+ (mapcar #'rexel--upload-size
                                                     uploads)))))
                  (seq-do-indexed (lambda (upload index)
                                    (rexel--upload-begin buf upload batch index count))
                                  uploads)))
            (error (when (rexel--upload-current-p buf (car uploads))
                     (rexel--upload-fail buf (error-message-string err))))))))))

(defun rexel--upload-fetch (buf upload)
  "Make the file of UPLOAD local and note its size.
Return nil if the batch in BUF ended during the fetch, which can block
while timers and filters run."
  (let* ((file (rexel--upload-file upload))
         (copy (rexel--local-copy file)))
    (if (not (rexel--upload-current-p buf upload))
        (when copy
          (ignore-errors (delete-file copy)))
      (when copy
        (setf (rexel--upload-file upload) copy
              (rexel--upload-temp upload) t))
      (or (setf (rexel--upload-size upload)
                (file-attribute-size
                 (file-attributes
                  (file-truename (rexel--upload-file upload)))))
          (error "Cannot read %s" file)))))

(defun rexel--local-copy (file)
  "Return a temporary local copy of the remote FILE, or nil for a local FILE.
A compressed file is copied as it is."
  (let ((inhibit-file-name-handlers
         (cons 'jka-compr-handler inhibit-file-name-handlers))
        (inhibit-file-name-operation 'file-local-copy))
    (file-local-copy file)))

(defun rexel--upload-begin (buf upload batch index count)
  "Request the ticket for UPLOAD in BUF and connect its stream with it.
With BATCH, UPLOAD is file INDEX of COUNT in the batch of that ID."
  (rexel--request
   rexel--conn "com.superlogical.terminal.upload_begin"
   `((session_id . ,rexel--session-id)
     (block_id . ,rexel--block-id)
     (args . ((ref . ,(rexel--upload-ref upload))
              (name . ,(rexel--upload-name upload))
              (size . ,(rexel--upload-size upload))
              ,@(when batch
                  `((batch . ,batch) (index . ,index) (count . ,count))))))
   (lambda (result err)
     (when (rexel--upload-current-p buf upload)
       (if err
           (rexel--upload-fail buf (format "%s: %s" (rexel--upload-name upload)
                                           (rexel--error-string err)))
         (with-current-buffer buf
           (condition-case err
               (rexel--upload-open buf upload
                                   (rexel--hello (alist-get 'ticket result)))
             (error (rexel--upload-fail buf (error-message-string err))))))))))

(defun rexel--upload-open (buf upload hello)
  "Connect the stream of UPLOAD in BUF to the data endpoint and send HELLO."
  (let ((ws (rexel--open-data-ws
             "rex.data.bytes.v1" hello
             (lambda (_ws frame)
               (rexel--on-upload-message buf upload
                                         (websocket-frame-payload frame)))
             (lambda (ws) (rexel--on-upload-close buf upload ws)))))
    (set-process-filter (websocket-conn ws) #'rexel--upload-filter)
    (setf (rexel--upload-ws upload) ws
          (rexel--upload-state upload) 'welcome)))

(defun rexel--upload-current-p (buf upload)
  "Return non-nil if UPLOAD is part of the batch in progress in BUF."
  (and (buffer-live-p buf)
       (memq upload (buffer-local-value 'rexel--uploads buf))))

(defun rexel--upload-filter (process output)
  "Keep the OUTPUT of an upload connection's PROCESS and pass it on.
websocket.el drops the status of a close frame; `rexel--close-status'
reads it from the kept bytes, which are few: a handshake and pings."
  (process-put process :upload-in
               (concat (process-get process :upload-in) output))
  (rexel--ws-filter process output))

(defun rexel--close-status (bytes)
  "Return the status of the close frame in BYTES, or nil if they have none.
BYTES are all a server sent on a WebSocket, with or without its
handshake response.  A close frame without a status gives 1005.
Frames websocket.el cannot read end the search."
  (let ((pos (if (string-prefix-p "HTTP/" bytes)
                 (+ 4 (or (string-search "\r\n\r\n" bytes) (length bytes)))
               0))
        (total (length bytes))
        status)
    (ignore-error websocket-unparseable-frame
      (catch 'websocket-incomplete-frame
        (while (and (not status) (< pos total))
          (let* ((len (websocket-get-payload-len
                       (substring bytes (1+ pos) (min total (+ pos 10)))))
                 (start (+ pos 1 (cdr len)))
                 (end (+ start (car len))))
            (when (> end total)
              (throw 'websocket-incomplete-frame nil))
            (when (= 8 (logand 15 (aref bytes pos)))
              (setq status (if (< (car len) 2)
                               1005
                             (logior (ash (aref bytes start) 8)
                                     (aref bytes (1+ start))))))
            (setq pos end)))))
    status))

(defun rexel--on-upload-message (buf upload bytes)
  "Handle BYTES, the server's answer to the HELLO of UPLOAD in BUF."
  (when (rexel--upload-current-p buf upload)
    (pcase (rexel--handshake bytes)
      ((and 'welcome (guard (eq (rexel--upload-state upload) 'welcome)))
       (setf (rexel--upload-state upload) 'ready)
       (rexel--upload-pump buf))
      ((and (pred integerp) reason)
       (rexel--upload-fail buf (format "%s: data connection rejected, reason %d"
                                       (rexel--upload-name upload) reason)))
      (_ (rexel--upload-fail buf "malformed data handshake")))))

(defun rexel--upload-pump (buf)
  "Send the first unfinished file of BUF's batch once its stream is ready.
Files go one at a time, in the order their paths are typed.  With all of
them done, report it and start the next batch."
  (with-current-buffer buf
    (let ((upload (seq-find (lambda (u) (not (eq (rexel--upload-state u) 'done)))
                            rexel--uploads))
          (count (length rexel--uploads)))
      (cond
       ((null upload)
        (rexel--upload-end)
        (message "rexel: uploaded %d file%s to %s"
                 count (if (= count 1) "" "s") (rexel--conn-name rexel--conn))
        ;; This runs in a sentinel, and fetching a remote file can block.
        (run-at-time 0 nil #'rexel--upload-next buf))
       ((eq (rexel--upload-state upload) 'ready)
        (setf (rexel--upload-state upload) 'sending)
        (rexel--upload-send-chunk buf upload))))))

(defun rexel--upload-pause (process)
  "Return the seconds to wait before the next chunk goes to PROCESS.
Timers due at once run in a row and Emacs reads no process output between
them, so once per `rexel--upload-slice' the next chunk waits 1 ms."
  (let ((now (float-time)))
    (if (< (- now (or (process-get process :upload-paused) 0)) rexel--upload-slice)
        0
      (process-put process :upload-paused now)
      0.001)))

(defun rexel--upload-send-chunk (buf upload)
  "Send the next chunk of UPLOAD in BUF, and the rest from a timer.
A chunk waits while `rexel--writes-overdue' holds writes."
  (when (rexel--upload-current-p buf upload)
    (if rexel--writes-overdue
        (run-at-time 0.01 nil #'rexel--upload-send-chunk buf upload)
      (let (returned)
        (unwind-protect
            (progn
              (condition-case err
                  (rexel--upload-write buf upload)
                ;; A batch that failed meanwhile closed the connection.
                (error (when (rexel--upload-current-p buf upload)
                         (rexel--upload-fail
                          buf (format "%s: %s" (rexel--upload-name upload)
                                      (error-message-string err))))))
              (setq returned t))
          ;; A `throw' through the write would end the chain of chunks.
          (when (and (not returned) (rexel--upload-current-p buf upload))
            (rexel--upload-fail
             buf (format "%s: the write was interrupted"
                         (rexel--upload-name upload)))))))))

(defun rexel--upload-write (buf upload)
  "Write the next chunk of UPLOAD in BUF and schedule the one after it.
An empty message after the last chunk ends the file.  Emacs reads no keys
while a chunk waits for room in the socket, so a chunk that stalls (see
`rexel--watched-write') fails the batch."
  (let* ((process (websocket-conn (rexel--upload-ws upload)))
         (sent (rexel--upload-sent upload))
         (end (min (rexel--upload-size upload)
                   (+ sent rexel--upload-chunk-bytes)))
         (chunk (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (when (< sent end)
                    (insert-file-contents-literally
                     (rexel--upload-file upload) nil sent end))
                  (buffer-string))))
    (unless (= (length chunk) (- end sent))
      (error "File changed during the upload"))
    (rexel--watched-write
     process
     (lambda ()
       (when (rexel--upload-current-p buf upload)
         (rexel--upload-fail buf (format "%s: the connection stalled"
                                         (rexel--upload-name upload)))))
     (lambda ()
       (process-send-string process (rexel--binary-frame chunk)))
     (length chunk))
    ;; A ping would wait behind the file's data; a chunk the socket
    ;; took shows the connection alive.
    (process-put process :last-read (float-time))
    (if (= sent end)
        (setf (rexel--upload-state upload) 'ending)
      (setf (rexel--upload-sent upload) end)
      (with-current-buffer buf
        (when rexel--upload-reporter
          (progress-reporter-update
           rexel--upload-reporter
           (apply #'+ (mapcar #'rexel--upload-sent rexel--uploads)))))
      (run-at-time (rexel--upload-pause process) nil
                   #'rexel--upload-send-chunk buf upload))))

(defun rexel--binary-frame (payload)
  "Return a client's binary WebSocket frame holding the unibyte PAYLOAD.
PAYLOAD is shorter than 65536 bytes.  The mask key is zero, which leaves
the payload as it is; masking it in `websocket-encode-frame' would take
most of an upload's time."
  (let ((len (length payload)))
    (concat (if (< len 126)
                (unibyte-string #x82 (logior #x80 len))
              (unibyte-string #x82 #xfe (ash len -8) (logand len #xff)))
            "\0\0\0\0" payload)))

(defun rexel--on-upload-close (buf upload ws)
  "Handle the close of WS, the connection of UPLOAD in BUF.
The server closes with status 1000 once it has the file, and the last of
a batch after typing the paths.  Any other close fails the batch."
  (when (rexel--upload-current-p buf upload)
    (let ((status (rexel--close-status
                   (or (process-get (websocket-conn ws) :upload-in) ""))))
      (if (and (eql status 1000) (eq (rexel--upload-state upload) 'ending))
          (progn
            (setf (rexel--upload-state upload) 'done)
            (rexel--upload-pump buf))
        (rexel--upload-fail
         buf (format "%s: %s" (rexel--upload-name upload)
                     (if status
                         (format "server closed the upload with status %d" status)
                       "connection lost")))))))

(defun rexel--upload-end ()
  "Forget this buffer's batch, delete its temporary copies, return the batch."
  (let ((uploads rexel--uploads))
    (setq rexel--uploads nil
          rexel--upload-reporter nil)
    (dolist (upload uploads)
      (when (rexel--upload-temp upload)
        (ignore-errors (delete-file (rexel--upload-file upload)))))
    uploads))

(defun rexel--upload-fail (buf reason)
  "Give up BUF's batch and its queued files, if any, reporting REASON.
A connection closed before the end of its file makes the server discard
the batch, so no path is typed."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (or rexel--uploads rexel--upload-queue)
        ;; Forgotten first, so that the closes below are ignored.
        (setq rexel--upload-queue nil)
        (let ((uploads (rexel--upload-end)))
          ;; A close frame would wait for room in a stalled connection.
          (dolist (upload uploads)
            (when-let* ((ws (rexel--upload-ws upload)))
              (ignore-errors (delete-process (websocket-conn ws)))))
          (message "rexel: %s: upload failed: %s" (buffer-name) reason))))))


;;; Downloads

(defvar rexel--downloads nil
  "The `rexel--download's in progress.")

(cl-defstruct (rexel--download (:constructor rexel--make-download) (:copier nil))
  "One file on its way from the machine a block runs on.
PATH is the file there and FILE the local file it becomes; its bytes
collect in TEMP beside FILE, RECEIVED of them so far, shown by REPORTER.
BUFFER is the block's buffer, CONN the control connection its ticket is
requested on and WS the WebSocket of its data stream.  STATE is `begin'
while the ticket is requested, `welcome' until the server answers the
HELLO, `receiving', then `done' or `failed'.
CALLBACK is called with FILE once it is whole."
  path file buffer conn temp ws state (received 0) reporter callback)

(defun rexel--download (path file &optional callback)
  "Copy PATH on the machine of this buffer's block to the local FILE.
Call CALLBACK with FILE once it is written."
  (let ((download (rexel--make-download
                   :path path :file file :buffer (current-buffer)
                   :conn rexel--conn :state 'begin :callback callback)))
    (rexel--request
     rexel--conn "com.superlogical.terminal.download_begin"
     `((session_id . ,rexel--session-id)
       (block_id . ,rexel--block-id)
       (args . ((ref . ,(concat "download:" (rexel--uuid)))
                (path . ,path))))
     (lambda (result err)
       (cond
        ((eq (rexel--download-state download) 'failed))
        (err (rexel--download-fail download (rexel--error-string err)))
        (t (with-current-buffer (rexel--download-buffer download)
             (condition-case err
                 (rexel--download-open download result)
               (error (rexel--download-fail
                       download (error-message-string err)))))))))
    (push download rexel--downloads)))

(defun rexel--download-open (download result)
  "Connect the stream of DOWNLOAD with the ticket in RESULT.
RESULT is the answer to `download_begin' for this buffer's block."
  (setf (rexel--download-temp download)
        (make-temp-file (concat (rexel--download-file download) ".") nil ".part")
        (rexel--download-reporter download)
        (make-progress-reporter
         (format "Downloading %s from %s..."
                 (alist-get 'name result) (rexel--conn-name rexel--conn))
         0 (max 1 (alist-get 'size result)))
        (rexel--download-state download) 'welcome
        (rexel--download-ws download)
        (rexel--open-data-ws
         "rex.data.bytes.v1" (rexel--hello (alist-get 'ticket result))
         (lambda (_ws frame)
           (rexel--on-download-message download
                                       (websocket-frame-payload frame)))
         (lambda (_ws) (rexel--download-fail download "connection lost")))))

(defun rexel--on-download-message (download bytes)
  "Handle BYTES received on the data connection of DOWNLOAD.
After the WELCOME every message is part of the file, and an empty one ends it."
  (pcase (rexel--download-state download)
    ('welcome
     (pcase (rexel--handshake bytes)
       ('welcome (setf (rexel--download-state download) 'receiving))
       ((and (pred integerp) reason)
        (rexel--download-fail
         download (format "data connection rejected, reason %d" reason)))
       (_ (rexel--download-fail download "malformed data handshake"))))
    ('receiving
     (if (string-empty-p bytes)
         (rexel--download-finish download)
       (condition-case err
           (progn
             (let ((coding-system-for-write 'binary))
               (write-region bytes nil (rexel--download-temp download) t 'quiet))
             (progress-reporter-update
              (rexel--download-reporter download)
              (cl-incf (rexel--download-received download) (length bytes))))
         (error (rexel--download-fail download (error-message-string err))))))))

(defun rexel--download-finish (download)
  "Move the received bytes of DOWNLOAD to its file and call its callback.
The file keeps the modes of the one it replaces."
  (let ((file (rexel--download-file download))
        (temp (rexel--download-temp download)))
    (when (condition-case err
              (progn
                (set-file-modes temp (or (file-modes file)
                                         (logand #o666 (default-file-modes))))
                (rename-file temp file t)
                t)
            (error (rexel--download-fail download (error-message-string err))
                   nil))
      (setf (rexel--download-state download) 'done)
      (setq rexel--downloads (delq download rexel--downloads))
      (message "rexel: downloaded %s" (abbreviate-file-name file))
      (when-let* ((callback (rexel--download-callback download)))
        (funcall callback file)))))

(defun rexel--download-fail (download reason)
  "Give up DOWNLOAD unless it has ended, reporting REASON."
  (unless (memq (rexel--download-state download) '(done failed))
    (setf (rexel--download-state download) 'failed)
    (setq rexel--downloads (delq download rexel--downloads))
    (when-let* ((ws (rexel--download-ws download)))
      (ignore-errors (delete-process (websocket-conn ws))))
    (when-let* ((temp (rexel--download-temp download)))
      (ignore-errors (delete-file temp)))
    (message "rexel: download of %s failed: %s"
             (rexel--download-path download) reason)))

(defun rexel--fail-downloads (buf reason)
  "Give up the downloads started from BUF, reporting REASON.
Return non-nil if there were any."
  (let ((downloads (cl-remove buf rexel--downloads
                              :key #'rexel--download-buffer :test-not #'eq)))
    (dolist (download downloads)
      (rexel--download-fail download reason))
    downloads))

(defun rexel--fail-transfers (conn)
  "Give up the uploads to CONN's blocks and its downloads without a ticket."
  (dolist (buf (buffer-list))
    (when (and (eq (buffer-local-value 'rexel--conn buf) conn)
               (buffer-local-value 'rexel--uploads buf))
      (rexel--upload-fail buf "control connection lost")))
  (dolist (download rexel--downloads)
    (when (and (eq (rexel--download-conn download) conn)
               (eq (rexel--download-state download) 'begin))
      (rexel--download-fail download "control connection lost"))))

(defconst rexel--list-dir-timeout 3
  "Seconds a prompt for a file waits for a directory's listing.")

(defvar rexel--file-history nil
  "History of the paths read by `rexel--read-file'.")

(defun rexel--list-dir (dir cache)
  "Return (PATH . NAMES) for DIR on the machine of this buffer's block.
PATH is DIR as an absolute path and NAMES are its entries, a directory
with a slash at its end.  The server reads a relative DIR in the
directory of the block's foreground process, and an empty one as that
directory.  CACHE is a hash table that keeps the answers by DIR.
Return nil when none arrives within `rexel--list-dir-timeout' seconds."
  (when (rexel--ready-p rexel--conn)
    (let ((process (websocket-conn (rexel--conn-ws rexel--conn))))
      (unless (gethash dir cache)
        (puthash dir 'requested cache)
        (rexel--request
         rexel--conn "com.superlogical.terminal.list_dir"
         `((session_id . ,rexel--session-id)
           (block_id . ,rexel--block-id)
           (args . ((path . ,dir) (files . t))))
         (lambda (result _err)
           (puthash dir
                    (cons (alist-get 'path result)
                          (mapcar (lambda (entry)
                                    (let ((name (alist-get 'name entry)))
                                      (if (alist-get 'file entry)
                                          name
                                        (concat name "/"))))
                                  (alist-get 'entries result)))
                    cache))))
      (let ((end (+ (float-time) rexel--list-dir-timeout)))
        (while (and (eq (gethash dir cache) 'requested)
                    (process-live-p process)
                    (< (float-time) end))
          (accept-process-output process 0.05)))
      (let ((answer (gethash dir cache)))
        (cond ((consp answer) answer)
              ;; The answer is lost with its connection.
              ((not (process-live-p process)) (remhash dir cache) nil))))))

(defun rexel--file-path (string)
  "Return the path that STRING, typed as a file name, stands for.
A // and a /~ start the path anew, as they do for `find-file'."
  (if (string-match ".*/\\(/\\|~\\(?:/\\|\\'\\)\\)" string)
      (substring string (match-beginning 1))
    string))

(defun rexel--file-table (buffer cache)
  "Return a completion table of the files on the machine of BUFFER's block.
It completes the last name of a path among the entries of its directory,
which `rexel--list-dir' keeps in CACHE."
  (lambda (string pred action)
    (let ((start (if (string-match ".*/" string) (match-end 0) 0)))
      (cond
       ((eq action 'metadata) '(metadata (category . file)))
       ((eq (car-safe action) 'boundaries)
        `(boundaries ,start . ,(or (string-search "/" (cdr action))
                                   (length (cdr action)))))
       (t
        (let ((dir (substring string 0 start)))
          (completion-table-with-context
           dir (cdr (with-current-buffer buffer
                      (rexel--list-dir (rexel--file-path dir) cache)))
           (substring string start) pred action)))))))

(defun rexel--read-file (prompt)
  "Read with PROMPT the path of a file on the machine of this buffer's block.
Completion starts in the directory of the block's foreground process, as
that of `read-file-name' does in `default-directory'.  The file name at
point is the default."
  (let* ((cache (make-hash-table :test #'equal))
         (listing (rexel--list-dir "" cache))
         (dir (and (car listing) (file-name-as-directory (car listing))))
         (minibuffer-completing-file-name t))
    (when dir
      (puthash dir listing cache))
    (rexel--file-path
     (completing-read prompt (rexel--file-table (current-buffer) cache) nil nil
                      dir 'rexel--file-history (thing-at-point 'filename t)))))


;;; Commands

(defun rexel-upload-file (files &optional buffer)
  "Send FILES to the program in the Rex block of BUFFER, by default this one.
The files are uploaded to the block's machine and their paths typed into
the terminal.  On this machine's server, local files are not uploaded.
Interactively, read a file in a block buffer; in a Dired buffer, send the
marked files to a block buffer read from the minibuffer."
  (interactive
   (if (derived-mode-p 'dired-mode)
       (list (dired-get-marked-files)
             (read-buffer "Upload to Rex block: " nil t
                          (lambda (b)
                            (buffer-local-value
                             'rexel--block-id
                             (if (consp b) (cdr b) (get-buffer b))))))
     (rexel--ensure-block-buffer-context)
     (list (list (read-file-name "Upload file: " nil nil t)))))
  (with-current-buffer (or buffer (current-buffer))
    (rexel--ensure-block-buffer-context)
    (rexel--send-files (mapcar #'expand-file-name files))))

(defun rexel-cancel-upload ()
  "Cancel the files being uploaded to the current block."
  (interactive)
  (unless (or rexel--uploads rexel--upload-queue)
    (user-error "No upload in progress"))
  (rexel--upload-fail (current-buffer) "cancelled"))

(defun rexel-cancel-download ()
  "Cancel the files being downloaded from the current block."
  (interactive)
  (unless (rexel--fail-downloads (current-buffer) "cancelled")
    (user-error "No download in progress")))

(defun rexel--download-target (path file)
  "Return the local file PATH is copied to when FILE is given for it.
A directory gets a file named as PATH's.  Signal a `user-error' for a
file on another machine and for one that cannot be written."
  (let ((file (expand-file-name file)))
    (when (file-remote-p file)
      (user-error "Not a local file: %s" file))
    (when (file-directory-p file)
      (setq file (file-name-concat file (file-name-nondirectory path))))
    (unless (file-writable-p file)
      (user-error "Cannot write %s" (abbreviate-file-name file)))
    file))

(defun rexel-download-file (path file &optional visit)
  "Copy PATH on the machine of the current block to the local FILE.
The server reads a relative PATH in the directory of the block's
foreground process, and a leading ~ as the home directory there.  A
directory given for FILE gets a file named as PATH's.  With VISIT, visit
FILE once it has arrived.
Interactively, read PATH with completion, starting in that directory,
and FILE.  A prefix argument is VISIT."
  (interactive
   (progn
     (rexel--ensure-block-buffer-context)
     (let* ((path (rexel--read-file "Download file: "))
            (file (if (string-empty-p (file-name-nondirectory path))
                      (user-error "No file to download")
                    (rexel--download-target
                     path
                     (read-file-name
                      "Save as: " (and (file-remote-p default-directory) "~/")
                      nil nil (file-name-nondirectory path))))))
       (when (and (file-exists-p file)
                  (not (y-or-n-p (format "File %s already exists; overwrite it? "
                                         (abbreviate-file-name file)))))
         (user-error "Not downloaded"))
       (list path file current-prefix-arg))))
  (rexel--ensure-block-buffer-context)
  (rexel--download
   path (rexel--download-target path file)
   (and visit
        (lambda (file)
          (rexel--prompt-later
           (lambda () (pop-to-buffer (find-file-noselect file))))))))

(provide 'rexel-transfer)
;;; rexel-transfer.el ends here
