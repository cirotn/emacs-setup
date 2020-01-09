;;(require 'git-util)
(require 'counsel)
(require 'vc-git)

(defun grep-auto-select ()
  "Runs grep on appropriate project type."
  (interactive)
  (let ((git-root (vc-git-root buffer-file-name)))
    (if (null git-root)
        (counsel-grep (thing-at-point 'word))
      ;;(grep-git-repository)
      (counsel-git-grep (thing-at-point 'word)))))

(provide 'grep-util)
