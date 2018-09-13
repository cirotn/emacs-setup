;; FILE: insert-code.el
;; DESC: Insert code into a file.

(defun ni-insert-code-file-header()
  "Creates a doxygen header for a file, at the top of the file."
  (interactive)
  (goto-char (point-min))
  (insert "/*!\n")
  (insert (concat "   \\file " (buffer-name) "\n"))
  (insert "   \\brief \n")
  (insert "*/\n")
  (insert "\n")
  (previous-line) (previous-line) (previous-line))

(defun ni-insert-code-include-guards()
  (interactive)
  (let* ((file-name (car (cdr (split-string buffer-file-name "source/"))))
         (no-slash (replace-regexp-in-string "/" "_" file-name))
         (no-period (replace-regexp-in-string "\\." "_" no-slash))
         (uppercase (upcase no-period))
         (guard (concat uppercase "_")))
    (save-excursion
      (insert (concat "#ifndef " guard "\n"))
      (insert (concat "#define " guard "\n"))
      (goto-char (point-max))
      (insert "\n")
      (insert (concat "#endif // " guard "\n")))))

(provide 'ni-emacs/insert-code)
