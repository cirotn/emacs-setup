;; List of user-specific stuff

(cond
 ((string-equal system-type "windows-nt")
  ;; Windows Setup
  ;;
  ;; Perforce variables
  (progn
    (defvar perforce-port "perforce.natinst.com:1666")
    (defvar penguin-port "penguin.natinst.com:1666")
    (defvar P4CLIENT "cnishigu_ctn_dt")
    (setq perforce-clientspec  "cnishigu_ctn-dt")
    (setq penguin-clientspec   "cnishigu_ctn-dt")
    (global-set-key (kbd "C-c o o") (lambda() (interactive) (find-file "C:/Users/cnishigu/Dropbox/orgmode")))
    ;; Exclude all files with boost in the file path from etags to keep file size manageable
    ;;(setq ni-ctags-ectags-options (concat ni-ctags-ectags-options " --exclude=boost")
    ;; Python path
    (setq python-shell-interpreter "C:/Users/cnishigu/AppData/Local/Programs/Python/Python36/pythonw.exe")
    ;; Removes shell warning about readline when starting python
    (setq python-shell-completion-native-enable nil)))
 ((string-equal system-type "darwin")
  ;; Mac OS Setup
  (progn
    ;; Python path
    (setq python-shell-interpreter "/anaconda3/bin/python3")
    ;; Removes shell warning about readline when starting python
    (setq python-shell-completion-native-enable nil)
    (global-set-key (kbd "C-c o o") (lambda() (interactive) (find-file "//Users/ciro/Dropbox/orgmode")))))
 (t
  ;; Setup for other systems (Linux)
  (progn
    (global-set-key (kbd "C-c o o") (lambda() (interactive) (find-file "~/Dropbox/orgmode"))))))

(provide 'my-config)
