;; Start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; no start up screen
(setq inhibit-startup-screen t)

;; show matching paren when cursor is on top
(show-paren-mode t)

;; show column number in mode bar
(setq column-number-mode t)

;; Shorten confirmation prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; C-v, C-c, C-x
(cua-mode t)

;; Set the annoying apps key as meta so I can use the same finger as mac
(setq w32-apps-modifier 'meta)

;; Set window title to currently visited buffer name instead of just "emacs"
(setq frame-title-format "%b")

;; Remove annoying beep
(setq visible-bell 1)

;; Truncate long lines by default
(setq-default truncate-lines t)

;; Highlight trailing whitespace
(setq-default show-trailing-whitespace t)

;; No default back-up
(setq make-backup-files nil)

;; Indentation
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq indent-line-function 'insert-tab)
(setq c-default-style "bsd" c-basic-offset 4)
(c-set-offset 'arglist-cont-nonempty '+)
(c-set-offset 'arglist-close 'c-lineup-close-paren)

;; Theme for terminals
(if (not (display-graphic-p)) (load-theme 'manoj-dark))

;; Remove buffer still has clients error due to using C-x k with client-server
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; Turn-off trailing whitespace highlight in minibuffer (makes ido-vertical look better)
(add-hook 'minibuffer-setup-hook (lambda() (setq show-trailing-whitespace nil)))

;; Turn-off trailing whitespace highlight in python inferior
(add-hook 'inferior-python-mode-hook (lambda() (setq show-trailing-whitespace nil)))

;; Turn-off trailing whitespace highlight in shell mode
(add-hook 'comint-mode-hook (lambda() (setq show-trailing-whitespace nil)))
(add-hook 'shell-mode-hook (lambda() (linum-mode -1)))

;; Set compilation mode options
(add-hook 'compilation-mode-hook (lambda ()
                                   (toggle-truncate-lines)
                                   (setq show-trailing-whitespace nil)))

;; Semantic
;; (semantic-mode 1)

;; (defun add-semantic-to-autocomplete()
;;   (add-to-list 'ac-sources 'ac-source-semantic))

;; (if (package-installed-p 'auto-complete)
;;     (add-hook 'c-mode-common-hook 'add-semantic-to-autocomplete))

(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))

(provide 'my-preferences)
