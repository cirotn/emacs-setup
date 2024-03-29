;; Start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; no start up screen
(setq inhibit-startup-message t)

;; show matching paren when cursor is on top
(show-paren-mode t)

;; Hide toolbar and menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)

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

;; Remove annoying beep, replace with a subtle flash of the mode line
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))

;; Truncate long lines by default
(setq-default truncate-lines t)

;; Highlight trailing whitespace
(setq-default show-trailing-whitespace t)

;; Keep back-up files out of the way
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Indentation
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq indent-line-function 'insert-tab)
(setq c-default-style "bsd" c-basic-offset 4)
(c-set-offset 'arglist-cont-nonempty '+)
(c-set-offset 'arglist-close 'c-lineup-close-paren)

(add-hook 'verilog-mode-hook (lambda()
                               (setq tab-width 2)
                               (setq verilog-indent-level 2)
                               (setq verilog-case-indent 2)
                               (setq verilog-cexp-indent 2)
                               (setq verilog-indent-level-behavioral 2)
                               (setq verilog-indent-level-declaration 2)
                               (setq verilog-indent-level-module 2)
                               (setq verilog-auto-newline nil)))

;; Theme for terminals
(if (not (display-graphic-p)) (load-theme 'wombat))

;; Turn-off trailing whitespace highlight in minibuffer (makes ido-vertical look better)
(add-hook 'minibuffer-setup-hook (lambda() (setq show-trailing-whitespace nil)))

;; Turn-off trailing whitespace highlight in python inferior
(add-hook 'inferior-python-mode-hook (lambda() (setq show-trailing-whitespace nil)))

;; Turn-off trailing whitespace highlight in shell mode
(add-hook 'comint-mode-hook (lambda() (setq show-trailing-whitespace nil)))
(add-hook 'eshell-mode-hook (lambda() (setq show-trailing-whitespace nil)))
(add-hook 'shell-mode-hook (lambda() (linum-mode -1)))
(add-hook 'cmake-mode-hook (lambda() (modify-syntax-entry ?_ "w")))

;; Turn-off trailing whitespace highlight in org-agenda
(add-hook 'org-agenda-mode-hook (lambda() (setq show-trailing-whitespace nil)))

;; Turn-off trailing whitespace highlight in calendar
(add-hook 'calendar-mode-hook (lambda() (setq show-trailing-whitespace nil)))

;; Set compilation mode options
(add-hook 'compilation-mode-hook (lambda()
                                   (toggle-truncate-lines)
                                   (setq show-trailing-whitespace nil)))

;; Relative line numbers
(setq display-line-numbers-type 'relative)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(provide 'my-preferences)
