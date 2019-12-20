(require 'vc-git)
(require 'util)

(defun grep-git-repository ()
  "Runs grep on git repository."
  (interactive)
  (let* ((pattern (prompt-user-for-word "Pattern"))
         (dir (vc-git-root buffer-file-name)))
    (compilation-start (concat "git --no-pager grep -i -I -n -e " pattern " -- " dir) 'grep-mode)))

(provide 'git-util)
