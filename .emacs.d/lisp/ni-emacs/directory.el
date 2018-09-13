;; FILE: directory.el
;; DESC: Functions related to directory navigation.

(require 'ni-emacs/util)

(defun ni-directory-open-file-in-explorer()
  "Open Windows folder pointing to the directory of the current buffer."
  (interactive)
  (progn
    (shell-command (concat "explorer " (replace-regexp-in-string "/" "\\" (file-name-directory (buffer-file-name)) t t)))
    (message "") ;; Clear annoying message about command returning 1
    ))

;; Test
;; (ni-directory-open-file-in-explorer)

;; TODO: this seems unused, remove it
(defun ni-files-in-directory (directory)
    "List the files in DIRECTORY and in its sub-directories."
    (let (files-list
          (current-directory-list (directory-files-and-attributes directory t)))
         (while current-directory-list
           (if (car (cdr (car current-directory-list))) ;; directory
             (if (equal "." (substring (car (car current-directory-list)) -1))
                 ();; skip . or ..
               (setq files-list
                     (append
                      (ni-files-in-directory
                       (car (car current-directory-list)))
                      files-list)))
               (setq files-list (cons (car (car current-directory-list)) files-list)))
           ;; move to the next filename in the list
           (setq current-directory-list (cdr current-directory-list)))
         ;; return the filenames
         files-list))

;; Test
;; (ni-files-in-directory "C:\\Windows\\System32\\drivers")

(defun ni-directory-path-correct (input-path)
  "Replaces any backslashes in a path to forward slashes.  Also,
makes sure that the last character is not a slash."
  (replace-regexp-in-string "/$"
                            ""
                            (replace-regexp-in-string "\\\\" "/" input-path)))

;; ni-directory-path-correct tests
;; (equal (ni-directory-path-correct "p:\\sa\\ss\\rpc\\trunk\\4.0")   "p:/sa/ss/rpc/trunk/4.0")
;; (equal (ni-directory-path-correct "p:\\sa\\ss\\rpc\\trunk\\4.0\\") "p:/sa/ss/rpc/trunk/4.0")
;; (equal (ni-directory-path-correct "p:/sa/ss/rpc/trunk/4.0/")       "p:/sa/ss/rpc/trunk/4.0")
;; (equal (ni-directory-path-correct "p:/sa/ss/rpc/trunk/4.0")        "p:/sa/ss/rpc/trunk/4.0")

(defun ni-directory-visit-next-file-with-base-name(&optional otherWindow)
  "Cycles between files with the same basename as the given file.
Usefull for cycling between header .h/.cpp/.ipp files etc."
  (interactive)
  (let* ((currentBufferFileName (replace-regexp-in-string "^.*/" "" (buffer-file-name)))
         (currentDirectory (replace-regexp-in-string "[a-zA-Z0-9._-]+$" "" (buffer-file-name)))
         (dotAtBeginning (equal ?\056 (aref currentBufferFileName 0)))
         (dotAnywhere (find ?\056 currentBufferFileName)))
    (unless (or dotAtBeginning (not dotAnywhere))
      (let* ((fileBaseName (replace-regexp-in-string "\\..*" "" currentBufferFileName))
             (matchFileList (directory-files currentDirectory t (concat "^" fileBaseName "\\.")))
             (identifiedFileList (member (buffer-file-name)
                                         matchFileList)))
        (unless (<= (length matchFileList) 1)
          (if (eq (length identifiedFileList) 1)
              (if otherWindow
                  (find-file-other-window (car matchFileList))
                (find-file (car matchFileList)))
            (if otherWindow
                (find-file-other-window (cadr identifiedFileList))
              (find-file (cadr identifiedFileList)))))))))

(defun ni-directory-visit-next-file-with-base-name-other-window ()
  (interactive)
  (ni-directory-visit-next-file-with-base-name t))

(provide 'ni-emacs/directory)
