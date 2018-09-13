(defun new-empty-buffer ()
  "Opens a new untitled buffer. Buffer will not ask to be saved when killed, be careful."
  (interactive)
  (switch-to-buffer (generate-new-buffer "untitled")))

(defun kill-all-buffers ()
    "Kill all buffers."
    (interactive)
    (mapc 'kill-buffer (buffer-list)))

(provide 'util)
