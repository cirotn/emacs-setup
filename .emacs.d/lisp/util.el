(defun new-empty-buffer ()
  "Opens a new untitled buffer. Buffer will not ask to be saved when killed, be careful."
  (interactive)
  (switch-to-buffer (generate-new-buffer "untitled")))

(defun kill-all-buffers ()
    "Kill all buffers."
   (interactive)
    (mapc 'kill-buffer (buffer-list)))

(defun prompt-user-for-word(prompt)
  "Prompts for input from the user, using the word at point as default value."
  (let ((default (thing-at-point 'word)))
    (read-string (concat prompt " (" default "): ") nil nil default)))

(defmacro with-file (file-path &rest body)
  "Executes BODY in a buffer visiting the file at FILE-PATH.  If
file at FILE-PATH is already open, use that buffer temporarily.
If not visiting the file already, the file will be visited
temporarily until the end of this macro.  The result of its last
embedded expression is returned."
  `(save-excursion
     (let ((file-buffer (find-buffer-visiting ,file-path))
           (buffer-created nil)
           (result-of-last-body-expr nil))
       (if file-buffer
           (set-buffer file-buffer)
         (find-file ,file-path)
         (setq buffer-created t))
       ,@(butlast body)
       (setq result-of-last-body-expr ,@(last body))
       (when buffer-created
         (kill-buffer file-buffer))
       result-of-last-body-expr)))

(defun open-file-folder-in-gui()
  "Open folder pointing to the directory of the current buffer."
  (interactive)
  (cond
    ((string-equal system-type "windows-nt")
     (shell-command (concat "explorer " (replace-regexp-in-string "/" "\\" (file-name-directory (buffer-file-name)) t t)))
     (message "")) ;; Clear annoying message about command returning 1
    ((string-equal system-type "darwin")
     (message "Not implemented yet"))
    (t
     ;; Setup for other systems (Linux)
     (shell-command (concat "xdg-open " (file-name-directory (buffer-file-name))) t t))
    ))

(defun display-current-buffer-path()
  "Prints path of current buffer in a message."
  (interactive)
  (message (buffer-file-name)))

(defun toggle-show-trailing-ws()
  (interactive)
  (setq show-trailing-whitespace (if show-trailing-whitespace nil t))
  (redraw-frame))

(provide 'util)
