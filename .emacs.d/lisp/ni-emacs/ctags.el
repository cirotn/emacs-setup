;; FILE: ctags.el
;; DESC: Functions to generate ctags including dependencies.

(require 'ni-emacs/util)
(require 'ni-emacs/component-dirs)
(require 'ni-emacs/package-file)

(defvar ni-ctags-exclusion-list nil)
(defvar ni-ctags-path-to-ectags "" "The path to the exhuberant ctags executable on your machine.")
(defvar ni-ctags-ectags-options "-e --recurse=yes --langmap=C++:+.ipp --exclude=boost" "The command line options used when generating ctags.")
(defvar ni-ctags-previous-tags-file nil "This variable contains the previous TAGS file used.")

(defun ni-ctags-gen()
  "Generate extended ctags for the current component
asynchronously.  This involves parsing the component's package
file and including the dependency's include dirs in the ctags
invocation."
  (interactive)
  (let ((root-dir (ni-component-dirs-root)))
    ;;(ni-ctags--gen-async root-dir '("boost_headers" "boost"))
    (ni-ctags--gen-async root-dir)
    ;; Give CTAGS a second to generate something
    (sleep-for 0.5)
    (ni-ctags-visit-tags-for-buffer)))

(defun ni-ctags-visit-tags-for-buffer()
  "Visits the TAGS file for the current buffer.  This function
assumes NI file layout."
  (interactive)
  (let* ((current-dir (ni-component-dirs-root))
         (tags-file-path (concat current-dir "/TAGS")))
    ;; By setting the tags-file-path, we change what TAGS files are
    ;; currently visited for the buffer.
    (if (and current-dir (file-exists-p tags-file-path))
        (setq tags-file-name tags-file-path)
        ;; TAGS file not found in the expected spot, try a generic lookup
      (when (buffer-file-name)
        (let ((generic-tags-location (ni-ctags--generic-find-tags-path
                                      (replace-regexp-in-string "[A-Za-z0-9._-]+$"
                                                                ""
                                                                (buffer-file-name)))))
          (if generic-tags-location
              (setq tags-file-name generic-tags-location)
            ;; Generic lookup failed, just use the previous tags visited
            (setq tags-file-name ni-ctags-previous-tags-file)))))))

(defun ni-ctags-visit-tags-table()
  (interactive)
  (visit-tags-table
   (concat (ni-component-dirs-root) "/TAGS")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ni-ctags-create-include-list(root-path)
  (let* ((package (concat root-path "/package"))
         (deps (ni-package-file-read-dep-name-list package)))
    (ni-util-list-with-string-appended
     (ni-package-file-read-dep-local-path-list package deps)
     "/includes")))

(defun ni-ctags--gen-async (root-path)
  "Generates extended ctags for the NI component that resides at
ROOT-PATH.  Extended ctags contain everything that standard ctags
have as well as information about immediate dependencies.  This
function calls the exhuberant ctags executable found at
'path-to-ectags'.  When ctags generation is complete, a message
will show in the minibuffer and a sound file will play."
  ;; Get the paths to the dependencies
  (let ((dep-path-includes (ni-ctags-create-include-list root-path))
        (ni-ctags-path-to-ectags  (expand-file-name "ctags.exe" ni-ctags-path-to-ectags)))
    (set-process-sentinel
     (start-process-shell-command (concat root-path "/TAGS")
                                  nil
                                  (concat "\"" ni-ctags-path-to-ectags "\"")
                                  (concat " " ni-ctags-ectags-options " ")
                                  (concat "-f \"" root-path "/TAGS\"" )
                                  (concat root-path "/source")
                                  (ni-util-join dep-path-includes " "))
     'ni-ctags--gen-sentinel)))

(defun ni-ctags--gen-sentinel(process event)
  "This function is called when asynchronous ctags generation
completes (either successfully or with errors)."
  (if (equal event "finished\n")
      (progn
        ;;(play-sound-file success-sound-path)
        (message "CTAGS generation finished!"))
    (progn
      ;;(play-sound-file failure-sound-path)
      (message "CTAGS generation had an error"))))

(defun ni-ctags--generic-find-tags-path (dir)
  "This function recursively looks for a TAGS file starting in
the DIR directory and going up 1 directory at a time until it
finds one (or has nowhere left to look)."
  (let ((corrected-dir (path-correct dir)))
    (cond
     ((equal corrected-dir "") nil)
     ((file-exists-p (concat corrected-dir "/TAGS"))
      (concat corrected-dir "/TAGS"))
     (t
      (ni-ctags--generic-find-tags-path (get-parent-dir corrected-dir))))))

(provide 'ni-emacs/ctags)
