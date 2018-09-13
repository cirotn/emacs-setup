;; FILE: perforce.el
;; DESC: Additional perforce queries.

(require 'ni-emacs/util)
(require 'ni-emacs/directory)

(defvar perforce-port
  "perforce.natinst.com:1666"
  "The location of the \"perforce\" server.  99.9% of the time, it should be \"perforce.natinst.com:1666\".")

(defvar penguin-port
  "penguin.natinst.com:1666"
  "The location of the \"penguin\" server.  99.9% of the time, it should be \"penguin.natinst.com:1666\".")

(defun ni-perforce-path-to-local-path (ni-perforce-path)
  ;; See if perforce-path has an explicit server in it...eg "perforce://sa/ss/blah"
  (let ((explicit-server (ni-util-string-match-regexp ni-perforce-path "^\\([a-zA-Z]+\\):" nil nil 1))
        (perforce-path (replace-regexp-in-string "^\\([a-zA-Z]+\\):" "" ni-perforce-path))
        (p4-where-additions ""))
    (cond
     ((null explicit-server)
      (setq p4-where-additions ""))
     ((string-equal "perforce" explicit-server)
      (setq p4-where-additions (concat "-c " perforce-clientspec " -p " perforce-port)))
     ((string-equal "penguin" explicit-server)
      (setq p4-where-additions (concat "-c " penguin-clientspec " -p " penguin-port)))
     (t (error "Unknown perforce server:%s" explicit-server)))
    (ni-directory-path-correct
     (ni-util-string-match-regexp (shell-command-to-string
                                   (concat "p4 " p4-where-additions " where " perforce-path))
                                  " \\([a-zA-Z]:.+\\)" nil nil 1))))

;; Tests
;; (ni-perforce-path-to-local-path "//sa/ss/rpc/trunk/4.0/package")
;; (ni-perforce-path-to-local-path "perforce://sa/ss/rpc/trunk/4.0/package")

(provide 'ni-emacs/perforce)
