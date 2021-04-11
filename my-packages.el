;; Hides minor modes from list to remove clutter
(use-package diminish
  :ensure t)

;; Doom theme has better integration with other packages
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-vibrant t))

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
        '((swiper . ivy--regex-plus)
          (counsel-git-grep-function . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
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
  :after ivy
  :config
  (ivy-rich-mode t))

;; Counsel provides a variety of functions using the same completion style as
;; ivy. Ivy only affects emacs functions that call completing-read-function,
;; counsel adds its own version of emacs commands that are not covered by ivy
;; alone. See counsel-faces for example, or counsel-git-grep.
(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x))
  :config
  ;; Don't start searches with ^
  (setq ivy-initial-inputs-alist nil))

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
  :init
  ;; Evil collection needs this set to nil before loading evil. Since this loads evil, need
  ;; set this here to avoid a warning.
  (setq evil-want-keybinding nil)
  :config
  (evil-leader/set-leader "<SPC>")
  (global-evil-leader-mode))

;; VI mode emulation
(use-package evil
  :ensure t
  :after evil-leader
  :init
  ;; Required by evil for undo and redo to work
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  ;; Undo system configuration above not quite working yet for redo
  (define-key evil-normal-state-map "u" 'undo-tree-undo)
  (define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)
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
  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit))

;; Swap buffers with another window. Also pulls in windmove, to move between
;; windows using a direction.
(use-package buffer-move
  :ensure t)

;; Snippets
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "C-c y") #'yas-expand)
  (setq yas-snippet-dirs '("~/emacs.d/snippets"
                           "~/yasnippets"))
  (yas-reload-all))

(use-package yasnippet-snippets
  :ensure t)

;; Configurable status bar with a nice look
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  ;; Don't tell me it's UTF-8 (could improve by only hiding if settings are as expected)
  (doom-modeline-buffer-encoding nil))

;; doom modeline requires this, run M-x all-the-icons-install-fonts
(use-package all-the-icons)

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

;; Projectile makes it easy to find files in project. Invoke find file using
;; C-c p f and find file dwim (to find a file at point) using C-c p g.
(use-package projectile
  :diminish
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

;; Hydra is useful for changing a set interactively
(use-package hydra
  :ensure t)

(defhydra hydra-zoom()
  "zoom"
  ("+" text-scale-increase "in")
  ("-" text-scale-decrease "out"))

(defhydra hydra-theme()
  "theme"
  ("a" (load-theme 'doom-acario-dark t) "acario-dark")
  ("A" (load-theme 'doom-acario-light t) "acario-light")
  ("c" (load-theme 'doom-city-lights t) "city-lights")
  ("C" (load-theme 'doom-oceanic-next t) "oceanic-next")
  ("d" (load-theme 'doom-dark+ t) "dark+")
  ("D" (load-theme 'doom-dracula t) "dracula")
  ("f" (load-theme 'doom-fairy-floss t) "fairy-floss")
  ("g" (load-theme 'doom-gruvbox t) "gruvbox")
  ("G" (load-theme 'doom-spacegrey t) "spacegrey")
  ("h" (load-theme 'doom-challenger-deep t) "challenger-deep")
  ("I" (load-theme 'doom-Iosvkem t) "Iosvkem")
  ("l" (load-theme 'doom-laserwave t) "laserwave")
  ("m" (load-theme 'doom-material t) "material")
  ("M" (load-theme 'doom-molokai t) "molokai")
  ("n" (load-theme 'doom-nord t) "nord")
  ("N" (load-theme 'doom-nord-light t) "nord-light")
  ("o" (load-theme 'doom-one t) "one")
  ("O" (load-theme 'doom-opera t) "opera")
  ("s" (load-theme 'doom-solarized-dark t) "solarized-dark")
  ("L" (load-theme 'doom-solarized-light t) "solarized-light")
  ("S" (load-theme 'doom-sourcerer t) "sourcerer")
  ("t" (load-theme 'doom-tomorrow-night t) "tomorrow-night")
  ("v" (load-theme 'doom-vibrant t) "vibrant")
  ("w" (load-theme 'wombat t) "wombat"))

(defhydra hydra-code()
  "code"
  ("b" blacken-buffer "blacken")
  ("e" conda-env-activate "conda activate"))

;; Help for keybindings
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; Git
(use-package magit
  :ensure t)

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; Language server protocol mode. Requires pip install python-language-server.
;; Also run pip install pyls-mypy and sudo apt install flake8.
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :custom
  (lsp-pyls-plugins-flake8-enabled t)
  (lsp-headerline-breadcrumb-enable nil)
  :config
  (add-hook 'python-mode-hook #'lsp))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom
  ;; Make help show within the window, not at the edge of the frame
  (lsp-ui-doc-alignment 'window)
  ;; Only show docstring using mouse cursor
  (lsp-ui-doc-show-with-cursor nil)
  ;; Disable sideline, distracting
  (lsp-ui-sideline-enable nil)
  ;; Don't highlight occurrences of the symbol at point
  (lsp-enable-symbol-highlighting nil))

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

(provide 'my-packages)
