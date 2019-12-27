(require 'cl)

(use-package ivy
  :ensure t
  :config
  (ivy-mode t)
  ;; Set the completion to be fuzzy for files and buffers
  (setq ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          (internal-complete-buffer . ivy--regex-fuzzy)
          (t . ivy--regex-plus)))
  ;; Adds recent files to buffer list
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package swiper
  :ensure t)

(use-package counsel
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
  ;; Make underscores part of a word
  (add-hook 'python-mode-hook '(lambda () (modify-syntax-entry ?_ "w")))
  (add-hook 'c++-mode-hook '(lambda () (modify-syntax-entry ?_ "w")))
  (add-hook 'c-mode-hook '(lambda () (modify-syntax-entry ?_ "w")))
  (add-hook 'makefile-mode-hook '(lambda () (modify-syntax-entry ?_ "w")))
  (add-hook 'package-mode-hook '(lambda () (modify-syntax-entry ?_ "w")))
  ;; Make hyphen part of a word in lisp
  (add-hook 'emacs-lisp-mode-hook '(lambda () (modify-syntax-entry ?- "w")))
  ;; Make asterisk not include \\<...\\>
  (define-key evil-normal-state-map (kbd "*") 'evil-search-unbounded-word-forward)
  ;; use space to scroll up and down
  (define-key evil-normal-state-map (kbd "SPC") 'evil-scroll-page-down)
  (define-key evil-normal-state-map (kbd "S-SPC") 'evil-scroll-page-up)
  ;; evil doesn't auto-indent in insert mode by default
  (define-key evil-insert-state-map (kbd "RET") 'evil-ret-and-indent)
  ;; set shift width according to language
  (add-hook 'python-mode-hook (lambda () (setq evil-shift-width python-indent)))
  (add-hook 'c++-mode-hook (lambda () (setq evil-shift-width c-basic-offset)))
  (add-hook 'c-mode-hook (lambda () (setq evil-shift-width c-basic-offset)))
  (add-hook 'makefile-mode-hook (lambda() (setq evil-shift-width c-basic-offset)))
  (add-hook 'package-mode-hook (lambda() (setq evil-shift-width 3)))
  (add-hook 'verilog-mode-hook (lambda() (setq evil-shift-width 2))))

(use-package iedit
  :ensure t)

;;(use-package p4
;;  :ensure t)

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
  (require 'my-powerline-theme)
  (if (display-graphic-p) (my-powerline-theme))
  (setq powerline-default-separator 'box))

;; When switching windows, number them and ask for the destination
(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-x o") 'ace-window))

(if (not (string-equal system-type "windows-nt"))
    (use-package clang-format
      :ensure t
      :config
      (setq clang-format-executable "clang-format-6.0")
      (global-set-key (kbd "C-c e r") 'clang-format-region)
      (global-set-key (kbd "C-c e b") 'clang-format-buffer)))

(use-package fill-column-indicator
  :ensure t
  :config
  (setq fci-rule-column 80)
  (setq fci-rule-width 4)
  (setq fci-rule-color "darkblue"))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1))

(use-package evil-magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

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

;; replaced by ivy
;;(use-package ido
;;  :ensure t
;;  :config
;;  (ido-mode t)
;;  ;; Disable automatic file name completion for recent files, this feature
;;  ;; causes the directory to change awkwardly while typing a file name
;;  (setq ido-auto-merge-work-directories-length -1)
;;  (setq ido-enable-flex-matching t))
;;
;;(use-package ido-vertical-mode
;;  :ensure t
;;  :config
;;  (ido-vertical-mode 1)
;;  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

;; Replaced by projectile
;; (use-package find-file-in-repository
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "C-x f") 'find-file-in-repository))

(require 'framemove)
(setq framemove-hook-into-windmove t)

(provide 'my-packages)
