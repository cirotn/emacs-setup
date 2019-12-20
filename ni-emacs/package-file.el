;; FILE: package-file.el
;; DESC: This file includes functions that help parse the package file.

(require 'ni-emacs/util)
(require 'ni-emacs/perforce)
(require 'ni-emacs/component-dirs)
(require 'ido)

(defun ni-package-file-open-dependency-dir(&optional otherWindow)
  "Opens local export directory of dependency."
  (interactive)
  (let ((dep (ni-package-file--pick-dependency)))
    (if dep
        (if otherWindow (find-file-other-window (cdr dep)) (find-file (cdr dep)))
      (error "Could not find dependency"))))

(defun ni-package-file-open-dependency-dir-other-window()
  "Opens local export directory of dependency."
  (interactive)
  (ni-package-file-open-dependency-dir t))

(defun ni-package-file-open-include(&optional otherWindow)
  "Looks for the file in the current #include line in current component and dependencies."
  (interactive)
  (or
   (ni-package-file--open-include-in-component otherWindow)
   (ni-package-file--open-include-in-dependency otherWindow)
   (message "Could not find file.")))

(defun ni-package-file-open-include-other-window()
  "Looks for the file in the current #include line in current component and dependencies."
  (interactive)
  (ni-package-file-open-include t))

(defun ni-package-file-read-dep-name-list(package-file-path)
  "Returns a list of all dependencies listed in the package file."
  (loop for i in (ni-package-file--read-deps-alist package-file-path)
        collect (car i)))

;; Test
;; (ni-package-file-read-dep-name-list "p:/sa/ss/apal/trunk/14.1/package")

(defun ni-package-file-read-dep-local-path-list(package-file-path deps)
  "Returns local paths for a list of dependencies."
  (let ((dep-strings (ni-package-file--read-dep-strs package-file-path)))
    (loop for dep in deps
          when (ni-package-file--find-dep-in-dep-str-list dep dep-strings)
          collect (ni-package-file--get-local-path-from-dep-string it))))

;; Test
;; (ni-package-file-read-dep-local-path-list "p:/sa/ss/apal/trunk/14.1/package" '("nitypes"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ni-package-file--pick-dependency()
  "Asks the user to select a dependency in the project. Returns a cons cell
   with (dep-name . dep-local-path)."
  (let* ((dep-list (ni-package-file--read-deps-alist))
         (dep-names (mapcar 'car dep-list))
         (dep (ido-completing-read "Dependency: " dep-names)))
    (assoc dep dep-list)))

(defun ni-package-file--find-dep-in-dep-str-list (dep-name dep-strings)
  "Returns a dependency string matching the given dependency"
  (find-if '(lambda (dep-string)
              (equal dep-name (ni-package-file--get-dep-name-from-dep-string dep-string)))
           dep-strings))

;; Test
;; (ni-package-file--find-dep-in-dep-str-list "nitypes" (ni-package-file--read-dep-strs "p:/sa/ss/apal/trunk/14.1/package"))

(defun ni-package-file--open-include-in-dependency(&optional otherWindow)
  "Looks for the file in the current #include line in dependencies exports."
  (let* ((file-str (ni-package-file--parse-c-include-file-name(thing-at-point 'line)))
         file-path)
    (loop for i in (ni-package-file--read-deps-alist) do ; i is (depname . deppath)
          (let ((path (concat (cdr i) "/includes/" file-str)))
            (if (file-exists-p path)
                (setq file-path path))))
    (if file-path
        (progn
          (message file-path)
          (if otherWindow (find-file-other-window file-path) (find-file file-path)))
      nil)))

(defun ni-package-file--open-include-in-component(&optional otherWindow)
  "Looks for the file in the current #include line in current component."
  (let ((file-str (ni-package-file--parse-c-include-file-name(thing-at-point 'line)))
        (cur-dir (file-name-directory (buffer-file-name)))
        (root (ni-component-dirs-root))
        file-path)
    (progn
      (setq file-path
            (or
             ;; Look for the file in the current directory
             (and (file-exists-p (concat cur-dir "/" file-str))
                  (concat cur-dir "/" file-str))
             ;; Look for it in includes, if this is an export
             (and (file-exists-p (concat root "/includes/" file-str))
                  (concat root "/includes/" file-str))
             ;; Look for it in source
             (and (file-exists-p (concat root "/source/" file-str))
                  (concat root "/source/" file-str))
             ;; Look for it in objects/codegen
             (and (file-exists-p (concat root "/objects/codegen/" file-str))
                  (concat root "/objects/codegen/" file-str))
             ;; Look for it in objects/export/includes
             (and (file-exists-p (concat root "/objects/export/includes/" file-str))
                  (concat root "/objects/export/includes/" file-str))))
    (if file-path
        (progn
          (message file-path)
          (if otherWindow (find-file-other-window file-path) (find-file file-path)))
      nil))))

(defun ni-package-file--parse-c-include-file-name(statement)
  "Returns the file name portion of a C #include statement."
  (progn (or (string-match "\"\\([^\"]+\\)\"" statement)
             (string-match "<\\([^>]+\\)>" statement)
             (error "Statement is not an include directive"))
         (match-string 1 statement)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deps-alist function (association list of dep-names and dep-local-path)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ni-package-file--read-deps-alist(&optional package-file)
  "Returns an association list of dependency names and local paths."
  (let* ((path (or package-file (ni-component-dirs-package-file-path)))
         (dep-strs (ni-package-file--read-dep-strs path))
         dep-list)
    (loop for dep-str in dep-strs
          collect (cons (ni-package-file--get-dep-name-from-dep-string dep-str)
                        (ni-package-file--get-local-path-from-dep-string dep-str)))))

;; Test
;; (ni-package-file--read-deps-alist "p:/sa/ss/apal/trunk/14.1/package")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dep-string functions (the package file fragments that specify a dependency)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ni-package-file--read-dep-strs (package-file-path)
  "Returns a list of dependency entry strings in a given package file."
  (with-file package-file-path
     (goto-char (point-min))
     (let ((dep-list nil))
       (while (re-search-forward "^\\(dependency [^}]+}\\)"
                                 nil
                                 t)
         (setq dep-list (cons (match-string 1)
                              dep-list)))

       dep-list)))

;; Test
;; (ni-package-file--read-dep-strs "p:/sa/ss/apal/trunk/14.1/package")

(defun ni-package-file--get-dep-name-from-dep-string(dep-string)
  (string-match "dependency +\\([A-Za-z0-9_]+\\)" dep-string)
  (match-string 1 dep-string))

;; Test
;; (ni-package-file--get-dep-name-from-dep-string "dependency test")

(defun ni-package-file--get-dep-path-from-dep-string(dep-string)
  (string-match "perforcePath *= *\\(.+\\);" dep-string)
  (match-string 1 dep-string))

;; Test
;; (ni-package-file--get-dep-path-from-dep-string "dependency test { perforcePath = test path; }")

(defun ni-package-file--get-local-path-from-dep-string(dep-string)
  (ni-perforce-path-to-local-path
   (ni-package-file--get-dep-path-from-dep-string dep-string)))

;; Test
;; (ni-package-file--get-local-path-from-dep-string "dependency test { perforcePath =//sa/ss; }")

(provide 'ni-emacs/package-file)
