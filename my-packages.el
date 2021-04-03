;; Hides minor modes from list to remove clutter
(use-package diminish
  :ensure t)

;; Use ivy for completion of commands
(use-package ivy
  :diminish
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

;; Provides description when invoking counsel-M-x. Use Alt-o + d to go function definition.
(use-package ivy-rich
  :ensure t
  :after
  (ivy)
  :config
  (ivy-rich-mode t))

;; Counsel provides a variety of functions using the same completion style as
;; ivy. Ivy only affects emacs functions that call completing-read-function,
;; counsel adds its own version of emacs commands that are not covered by ivy
;; alone. See counsel-faces for example, or counsel-git-grep.
(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)))

;; Undo-tree for storing and displaying multiple undo branches
(use-package undo-tree
  :diminish
  :ensure t
  :config
  ;; No trailing whitespace on undo-tree
  (add-hook 'undo-tree-visualizer-mode-hook (lambda() (setq show-trailing-whitespace nil)))
  (global-undo-tree-mode))

;; Use space as a leader. Should come before evil.
(use-package evil-leader
  :ensure t
  :config
  (evil-leader/set-leader "<SPC>")
  (global-evil-leader-mode))

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
  (add-hook 'yaml-mode-hook '(lambda () (modify-syntax-entry ?_ "w")))
  ;; Make hyphen part of a word in lisp
  (add-hook 'emacs-lisp-mode-hook '(lambda () (modify-syntax-entry ?- "w")))
  ;; Make asterisk not include \\<...\\>
  (define-key evil-normal-state-map (kbd "*") 'evil-search-unbounded-word-forward)
  ;; evil doesn't auto-indent in insert mode by default
  (define-key evil-insert-state-map (kbd "RET") 'evil-ret-and-indent)
  ;; set shift width according to language
  (add-hook 'python-mode-hook (lambda () (setq evil-shift-width python-indent)))
  (add-hook 'c++-mode-hook (lambda () (setq evil-shift-width c-basic-offset)))
  (add-hook 'c-mode-hook (lambda () (setq evil-shift-width c-basic-offset)))
  (add-hook 'makefile-mode-hook (lambda() (setq evil-shift-width c-basic-offset)))
  (add-hook 'package-mode-hook (lambda() (setq evil-shift-width 3)))
  (add-hook 'verilog-mode-hook (lambda() (setq evil-shift-width 2)))
  ;; Make escape exit minibuffer instead of requiring Ctrl-g.
  ;; This is under evil because I think it needs to be loaded after evil
  ;; loads, but haven't verified that.
  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
  ;; Evil requires that you choose an undo system
  (define-key evil-normal-state-map "u" 'undo-tree-undo)
  (define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo))

;; Edit mulitple occurrences of a string in a buffer at once. Invoke using c-;.
(use-package iedit
  :ensure t)

;; Basic emacs auto-complete
(use-package auto-complete
  :diminish
  :ensure t
  :config
  (ac-config-default))

;; Swap buffers with another window. Also pulls in windmove, to move between
;; windows using a direction.
(use-package buffer-move
  :ensure t)

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
  (evil-leader/set-key "p" 'projectile-command-map)
  ;; Set alternative key binding for find file
  (global-set-key (kbd "C-x f") 'projectile-find-file)
  ;; Use ivy to select the file
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1))

;; The Emacs python development environment
(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  :config
  (setq elpy-rpc-python-command "/home/ciro/miniconda3/bin/python")
  (add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))
  ;; Workaround for an issue that prints 'error in process filter' and hangs
  ;; for a few seconds when typing temporarily unbalanced strings
  (delete 'elpy-module-highlight-indentation elpy-modules))

;; Use conda for python
(use-package conda
  :ensure t
  :init
  (setq conda-anaconda-home (expand-file-name "~/miniconda3"))
  (setq conda-env-home-directory (expand-file-name "~/miniconda3")))

;; Python formatting
(use-package blacken
  :ensure t
  :config
  (setq blacken-skip-string-normalization t)
  (setq blacken-line-length 120))

;; Yaml file mode
(use-package yaml-mode
  :ensure t)

;; Framemove was not working with package manager at one point
;;(use-package framemove :ensure t
;;  :config
;;  (setq framemove-hook-into-windmove t))

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
