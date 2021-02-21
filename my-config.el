;; List of user-specific stuff

(cond
 ((string-equal system-type "windows-nt")
  ;; Windows Setup
  ;;
  ;; Perforce variables
  (progn
    (global-set-key (kbd "C-c o o") (lambda() (interactive) (find-file "C:/Users/cnishigu/orgmode")))
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
    (global-set-key (kbd "C-c o o") (lambda() (interactive) (find-file "//Users/ciro/orgmode")))))
 (t
  ;; Setup for other systems (Linux)
  (progn
    (global-set-key (kbd "C-c o o") (lambda() (interactive) (find-file "~/orgmode"))))))

(provide 'my-config)
