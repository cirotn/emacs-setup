(require 'cl)

;; Use ivy for completion of commands
(use-package ivy
  :ensure t
  :config
  (ivy-mode t)
  ;; Set the number of minibuffer lines
  (setq ivy-height 20)
  ;; Set the completion to be fuzzy for files and buffers
  (setq ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          (internal-complete-buffer . ivy--regex-fuzzy)
          (t . ivy--regex-plus)))
  ;; Adds recent files to buffer list
  (setq ivy-use-virtual-buffers t)
  ;; Configure display of number of options
  (setq ivy-count-format "(%d/%d) "))

;; Swiper provides a search (within a buffer) that improves on isearch.
;; Invoke using M-x swiper-isearch.
(use-package swiper
  :ensure t)

;; Counsel provides a variety of functions using the same completion style as
;; ivy. Ivy only affects emacs functions that call completing-read-function,
;; counsel adds its own version of emacs commands that are not covered by ivy
;; alone. See counsel-faces for example, or counsel-git-grep.
(use-package counsel
  :ensure t)

;; Undo-tree for storing and displaying multiple undo branches
(use-package undo-tree
  :ensure t
  :config
  ;; No trailing whitespace on undo-tree
  (add-hook 'undo-tree-visualizer-mode-hook (lambda() (setq show-trailing-whitespace nil)))
  (global-undo-tree-mode))

;; VI mode emulation
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

;; Edit mulitple occurrences of a string in a buffer at once. Invoke using c-;.
(use-package iedit
  :ensure t)

;; Use relative line numbers
(use-package linum-relative
  :ensure t
  :config
  (global-linum-mode t)
  (linum-relative-on))

;; Basic emacs auto-complete
(use-package auto-complete
  :ensure t
  :config
  (ac-config-default))

;; Swap buffers with another window. Also pulls in windmove, to move between
;; windows using a direction.
(use-package buffer-move
  :ensure t
  :config
  ;; Keymap for windmove that use VI direction keys
  (global-set-key (kbd "C-c h") 'windmove-left)
  (global-set-key (kbd "C-c l") 'windmove-right)
  (global-set-key (kbd "C-c k") 'windmove-up)
  (global-set-key (kbd "C-c j") 'windmove-down)
  ;; Keymap for buffer move that use VI direction keys
  (global-set-key (kbd "C-c H") 'buf-move-left)
  (global-set-key (kbd "C-c L") 'buf-move-right)
  (global-set-key (kbd "C-c K") 'buf-move-up)
  (global-set-key (kbd "C-c J") 'buf-move-down))

;; Configurable status bar with a nice look
(use-package powerline
  :ensure t
  :config
  ;; Using my own custom theme for powerline, but only in graphical displays
  (require 'my-powerline-theme)
  (if (display-graphic-p) (my-powerline-theme))
  (setq powerline-default-separator 'box))

;; When switching windows, number them and ask for the destination
(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-x o") 'ace-window))

;; Use clang-format if not on Windows
(if (not (string-equal system-type "windows-nt"))
    (use-package clang-format
      :ensure t
      :config
      (setq clang-format-executable "clang-format-6.0")
      (global-set-key (kbd "C-c f r") 'clang-format-region)
      (global-set-key (kbd "C-c f b") 'clang-format-buffer)))

;; Optionally show a ruler at column 80. Invoke using C-c w r.
(use-package fill-column-indicator
  :ensure t
  :config
  (setq fci-rule-column 80)
  (setq fci-rule-width 4)
  (setq fci-rule-color "darkblue")
  (global-set-key (kbd "C-c w r") 'fci-mode))

;; Projectile makes it easy to find files in project. Invoke find file using
;; C-c p f and find file dwim (to find a file at point) using C-c p g.
(use-package projectile
  :ensure t
  :config
  ;; Install command shortcuts under C-c p
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  ;; Set alternative key binding for find file
  (global-set-key (kbd "C-x f") 'projectile-find-file)
  ;; Use ivy to select the file
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1))

;; Git porcelain with key bindings that work with evil
(use-package evil-magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

;; Disabling due to lack of use
;;(use-package p4
;;  :ensure t)

;; Framemove was not working with package manager at one point
;;(use-package framemove :ensure t
;;  :config
;;  (setq framemove-hook-into-windmove t))

;; Disabling due to lack of use
;; (require 'framemove)
;; (setq framemove-hook-into-windmove t)

;; Disabling due to lack of use
;; (use-package yasnippet :ensure t
;;  :config
;;     ;;Only load yasnippets from my own directory
;;     (setq-default yas-snippet-dirs "~/.emacs.d/snippets")
;;     (yas-global-mode 1)
;;     ;; Make it triggered by shift-tab so it doesn't conflict with auto-complete
;;     (define-key yas-minor-mode-map (kbd "<tab>") nil)
;;     (define-key yas-minor-mode-map (kbd "TAB") nil)
;;     (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand))

;; Replaced by ivy
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

(provide 'my-packages)
