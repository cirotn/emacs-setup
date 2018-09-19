(require 'vc-git)
(require 'util)

(defun grep-git-repository ()
  "Runs grep on git repository."
  (interactive)
  (let* ((pattern (prompt-user-for-word "Pattern"))
         (dir (vc-git-root buffer-file-name)))
    (with-file dir (grep (concat "grep -nH -ir --color " pattern " .")))))

(provide 'git-util)
