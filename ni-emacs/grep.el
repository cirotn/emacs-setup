;; FILE: grep.el
;; DESC: Functions related to grep.

(require 'ni-emacs/util)
(require 'ni-emacs/component-dirs)

(defun ni-grep-interactive()
  "Runs grep on selected destination."
  (interactive)
  (let* ((pattern (ni-util-prompt-user-for-word "Pattern"))
         (dir-choices (list "component source" "current directory"))
         (menu-str "")
         (dir-selection
          (progn
            ;; Loop through list of dir-choices incrementing y for each one
            (loop for x in dir-choices for y from 0
                  when (> y 0) do (setq menu-str (concat menu-str " "))
                  do (setq menu-str (concat menu-str (format "[%d] %s" y x))))
            (nth (read-number (concat menu-str ": ") 0) dir-choices))))
    (cond
     ((equal dir-selection "component source") (ni-grep--grep-component-source-dir pattern))
     ((equal dir-selection "current directory") (ni-grep--grep-current-dir-recursive pattern))
     (t (error "Invalid selection")))))

;;;;;;;;;;;;;;;;;;;;;;
;; Private functions
;;;;;;;;;;;;;;;;;;;;;;

(defun ni-grep--grep(base-dir sub-dir pattern recursive)
  (if recursive
      (with-file base-dir (grep (concat "grep -nH -ir " pattern " " sub-dir)))
    ;; else
    (with-file base-dir (grep (concat "grep -nH -i " pattern " " sub-dir)))))

;; Test
;; (ni-grep "." "tools" "test" nil)

(defun ni-grep--grep-current-dir-recursive(pattern)
  "Greps the current directory."
  (ni-grep--grep (file-name-directory (buffer-file-name)) "*" pattern t))

;; Test
;; (ni-grep-current-dir "recursive")

(defun ni-grep--grep-component-source-dir(pattern)
  "Greps the specified directory of the current component recursively."
  (ni-grep--grep (ni-component-dirs-root) "source" pattern t))

(provide 'ni-emacs/grep)
