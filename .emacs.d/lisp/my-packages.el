(require 'cl)

(use-package iedit
  :ensure t)

(use-package undo-tree
  :ensure t
  :config
  (add-hook 'undo-tree-visualizer-mode-hook (lambda() (setq show-trailing-whitespace nil)))
  (global-undo-tree-mode))

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  ;; Make evil forward work with symbols rather than words, in whatever way
  ;; symbols are defined in the current emacs mode
  (add-hook 'python-mode-hook '(lambda () (modify-syntax-entry ?_ "w")))
  (add-hook 'c++-mode-hook '(lambda () (modify-syntax-entry ?_ "w")))
  (add-hook 'c-mode-hook '(lambda () (modify-syntax-entry ?_ "w")))
  (add-hook 'makefile-mode-hook '(lambda () (modify-syntax-entry ?_ "w")))
  (add-hook 'package-mode-hook '(lambda () (modify-syntax-entry ?_ "w")))
  ;; Leave M-. for tag, to allow use of C-u M-. for next tag (seems to work better than C-])
  (define-key evil-normal-state-map (kbd "M-.") nil)
  ;; Use space to scroll up and down
  (define-key evil-normal-state-map (kbd "SPC") 'evil-scroll-page-down)
  (define-key evil-normal-state-map (kbd "S-SPC") 'evil-scroll-page-up)
  ;; Evil doesn't auto-indent in insert mode by default
  (define-key evil-insert-state-map (kbd "RET") 'evil-ret-and-indent)
  ;; Set shift width according to language
  (add-hook 'python-mode-hook (lambda () (setq evil-shift-width python-indent)))
  (add-hook 'c++-mode-hook (lambda () (setq evil-shift-width c-basic-offset)))
  (add-hook 'c-mode-hook (lambda () (setq evil-shift-width c-basic-offset)))
  (add-hook 'makefile-mode-hook (lambda() (setq evil-shift-width c-basic-offset)))
  (add-hook 'package-mode-hook (lambda() (setq evil-shift-width 3)))
  (add-hook 'verilog-mode-hook (lambda() (setq evil-shift-width 2))))

(use-package ido
  :ensure t
  :config
  (ido-mode t)
  ;; Disable automatic file name completion for recent files, this feature
  ;; causes the directory to change awkwardly while typing a file name
  (setq ido-auto-merge-work-directories-length -1)
  (setq ido-enable-flex-matching t))

(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package p4
  :ensure t)

(use-package linum-relative
  :ensure t
  :config
  (global-linum-mode t)
  (linum-relative-on))

(use-package auto-complete
  :ensure t
  :config
  (ac-config-default))

(use-package buffer-move
  :ensure t)

(use-package powerline
  :ensure t
  :config
  (if (display-graphic-p) (powerline-default-theme))
  (setq powerline-default-separator 'box))

(use-package find-file-in-repository
  :ensure t
  :config
  (global-set-key (kbd "C-x f") 'find-file-in-repository))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-x o") 'ace-window))

;;(use-package projectile
;;  :ensure t
;;  :config
;;  (projectile-mode))

;;(use-package magit
;;  :ensure t
;;  :config
;;  (global-set-key (kbd "C-x g") 'magit-status))

;;(use-package framemove :ensure t
;;  :config
;;  (setq framemove-hook-into-windmove t))

;; (use-package yasnippet :ensure t
;;  :config
;;     ;;Only load yasnippets from my own directory
;;     (setq-default yas-snippet-dirs "~/.emacs.d/snippets")
;;     (yas-global-mode 1)
;;     ;; Make it triggered by shift-tab so it doesn't conflict with auto-complete
;;     (define-key yas-minor-mode-map (kbd "<tab>") nil)
;;     (define-key yas-minor-mode-map (kbd "TAB") nil)
;;     (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand))

(require 'framemove)
(setq framemove-hook-into-windmove t)

(provide 'my-packages)
