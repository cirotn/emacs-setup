;; FILE: component-dirs.el
;; DESC: Functions related to build-tool component directories.

(require 'cl) ;; loop
(require 'ido)
(require 'ni-emacs/util)

(defun ni-component-dirs-file-open-source-file(&optional otherWindow)
  "Opens a file in the source directory"
  (interactive)
  (let* ((root-dir (ni-component-dirs-root))
         (source-dir (concat root-dir "/source"))
         (file-list (ni-util-directory-files-recursive source-dir))
         (name-list (mapcar 'file-name-nondirectory file-list))
         (file-name (ido-completing-read "File: " name-list))
         (file-path))
    (progn
      (loop for i in file-list do
	    (if (string= (file-name-nondirectory i) file-name)
		(setq file-path i)))
      (if file-path
          (if otherWindow (find-file-other-window file-path) (find-file file-path))
	(error "File not found")))))

(defun ni-component-dirs-file-open-source-file-other-window()
  "Opens a file in the source directory"
  (interactive)
  (ni-component-dirs-file-open-source-file t))

(defun ni-component-dirs-open-package(&optional otherWindow)
  "Opens package file."
  (interactive)
  (if otherWindow
      (find-file-other-window (ni-component-dirs-package-file-path))
    (find-file (ni-component-dirs-package-file-path))))

(defun ni-component-dirs-open-package-other-window()
  "Opens package file in other window."
  (interactive)
  (ni-component-dirs-open-package t))

(defun ni-component-dirs-open-makefile()
  "Opens a makefile in the current component."
  (interactive)
  (let* ((root (ni-component-dirs-root))
         (files (append (file-expand-wildcards (concat root "/*.mak")) (list (concat root "/Makefile"))))
         (short-names (mapcar 'file-name-sans-extension (mapcar 'file-name-nondirectory files)))
         (menu-str ""))
    (progn
      ;; Loop through list of choices incrementing y for each one
      (loop for x in short-names for y from 0
            when (> y 0) do (setq menu-str (concat menu-str " "))
            do (setq menu-str (concat menu-str (format "[%d] %s" y x))))
      (find-file (nth (read-number (concat menu-str ": ") 0) files)))))

(defun ni-component-dirs-root(&optional path)
  "Return the NI root directory of a given file path. The NI root directory is
directory that contains the package file, of either a trunk or an export."
  (if (not path)
      (if (buffer-file-name)
          (setq path (file-name-directory (buffer-file-name)))))
  (if path
      (or (and (file-exists-p (concat path "/package"))
               path)
          ;; Check if we are either in "source", "objects", "includes", or in
          ;; a subdirectory of one of those.
          (loop for dir in '("/source" "/objects" "/includes") do
                (and (posix-string-match dir path)
                     (file-exists-p (concat (substring path 0 (match-beginning 0)) "/package"))
                     (return (substring path 0 (match-beginning 0))))))))

(defun ni-component-dirs-package-file-path()
  "Returns local path to package file."
  (concat (ni-component-dirs-root) "/package"))

(provide 'ni-emacs/component-dirs)
