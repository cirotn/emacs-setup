;; FILE: p4login.el
;; DESC: Function to login to p4.

(defun ni-p4login()
  "Logs in to perforce and penguin servers using p4 login command."
  (interactive)
  (let* ((shellBufferName "P4 login")
         (pwd (read-passwd "Password: "))
         (tempBatchFileName (ni-p4login--make-batch-file
                            (list "@echo Logging in to Perforce server..."
                                  "@echo off"
                                  (concat "echo " pwd "|p4 -p perforce:1666 login\n")
                                  "@echo Logging in to Penguin server..."
                                  (concat "echo " pwd "|p4 -p penguin:1666 login\n")))))
    (switch-to-buffer-other-window shellBufferName)
    (call-process-shell-command tempBatchFileName nil shellBufferName tempBatchFileName)
    (delete-file tempBatchFileName)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ni-p4login--make-batch-file(commands)
  (save-excursion
    (let (tempFileName)
    (with-temp-file (setq tempFileName (make-temp-file "tempLogin" nil ".bat"))
      (mapc 'insert (mapcar '(lambda (cmd) (concat cmd "\n")) commands)))
    tempFileName)))

(provide 'ni-emacs/p4login)
