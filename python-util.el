(defun python-util-new-empty-buffer ()
  "Opens a new untitled buffer in python mode. Buffer will not ask to be saved when killed, be careful."
  (interactive)
  (switch-to-buffer (generate-new-buffer "untitled"))
  ;; Start in python mode by default
  (python-mode))

(defun python-debug-with-pdb()
  (interactive)
  (if nil (buffer-file-name)
    (write-file (concat temporary-file-directory "debug.py")))
  (pdb (concat python-shell-interpreter " -m pdb " buffer-file-name)))

(provide 'python-util)
