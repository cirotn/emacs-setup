;; FILE: build.el
;; DESC: Functions related to build tools.

(defvar ni-build-history
  (setq ni-build-history (make-hash-table :test 'equal))
  "Hash that stores the most recent \"make\" command run on a given trunk.")

(defun ni-build()
  "Build the component that a given file belongs to."
  (interactive)
  (let* ((rootDir (ni-component-dirs-root))
         (defaultArgs (gethash rootDir ni-build-history)))
    (ni-build--component rootDir (read-string
                                  (concat "Make args"
                                          (if defaultArgs
                                              (concat " (\"" defaultArgs "\"): ")
                                            ": "))
                                  nil nil defaultArgs))))

(defun ni-build-with-selection()
  "Build the component that a given file belongs to, asks to select a target."
  (interactive)
  (let* ((rootDir (ni-component-dirs-root))
         (makefiles-full-name (file-expand-wildcards (concat rootDir "/*.mak")))
         (components (mapcar 'file-name-sans-extension (mapcar 'file-name-nondirectory makefiles-full-name)))
         (debug-or-release '("." "debug" "release"))
         (menu-str "")
         (make-arg-str "TARGET="))
    (progn
      ;; Choose a specific component from list of *.mak files in root
      (setq components (append components '(".")))
      (loop for x in components for y from
            0 when (> y 0)
            do (setq menu-str (concat menu-str " "))
            do (setq menu-str (concat menu-str (format "[%d] %s" y x))))
      (setq make-arg-str (concat make-arg-str (nth (read-number (concat menu-str ": ") 0) components)))
      ;; For now, just build all OSENVs, processors, and compilers
      (setq make-arg-str (concat make-arg-str "/./././"))
      ;; Choose debug or release
      (setq menu-str "")
      (loop for x in debug-or-release for y from
            0 when (> y 0)
            do (setq menu-str (concat menu-str " "))
            do (setq menu-str (concat menu-str (format "[%d] %s" y x))))
      (setq make-arg-str (concat make-arg-str (nth (read-number (concat menu-str ": ") 0) debug-or-release)))
      (ni-build--component rootDir make-arg-str))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ni-build--make-batch-file(commands)
  (save-excursion
    (let (tempFileName)
    ;;(with-temp-file (setq tempFileName (make-temp-file "c:\\temp\\tempBuild" nil ".bat")) -- for debugging the script
    (with-temp-file (setq tempFileName (make-temp-file "tempBuild" nil ".bat"))
      (progn
        (message "%s" tempFileName)
        (mapc 'insert (mapcar '(lambda (cmd)
                               (concat cmd "\n"))
                            commands))))
    tempFileName)))
;; MANUAL ni-build--make-batch-file tests
;; (ni-build--make-batch-file '("p:" "cd blah"))

(defun ni-build--clear-buffer()
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun ni-build--component(rootDir makeArgs)
  (let ((shellBufferName (concat "*BUILD_"
                                 (replace-regexp-in-string ".*/\\([A-Za-z0-9_-]+\\)/\\(trunk\\|development\\)/\\([0-9.]+\\).*" "\\1_\\2_\\3" rootDir)
                                 "*"))
        (tempBatchFileName (ni-build--make-batch-file
                            (list "@echo on"
                                  "p:"
                                  (concat "cd "
                                          (replace-regexp-in-string "/" "\\\\" rootDir))
                                  "call setupEnv.bat"
                                  "@echo on"
                                  (concat "make " makeArgs)))))
    (switch-to-buffer-other-window shellBufferName)
    (ni-build--clear-buffer)
    (set-process-sentinel
     (start-process-shell-command tempBatchFileName shellBufferName tempBatchFileName)
     'ni-build--sentinel)
    (puthash rootDir makeArgs ni-build-history)
    (compilation-mode)
    (end-of-buffer)))

(defun ni-build--sentinel (proc change)
  (if (string= (substring change 0 8) "finished")
      (message "Build Succeeded!")
    (message "Build Error:\"%s\"" (chomp change)))
  ;; delete temporary batch file
  (delete-file (process-name proc)))

(provide 'ni-emacs/build)
