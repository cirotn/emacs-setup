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

(provide 'util)
