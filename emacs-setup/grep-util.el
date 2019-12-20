(require 'git-util)
(require 'ni-emacs/grep)
(require 'vc-git)

(defun grep-auto-select ()
  "Runs grep on appropriate project type."
  (interactive)
  (let ((git-root (vc-git-root buffer-file-name)))
    (if (null git-root)
        (ni-grep-interactive)
      (grep-git-repository))))

(provide 'grep-util)
