;; Find file
(evil-leader/set-key
  "<SPC>" 'projectile-find-file)

(global-set-key (kbd "C-c <SPC>") 'projectile-find-file)

;; Execute
(evil-leader/set-key
  ":" 'counsel-M-x)

(global-set-key (kbd "C-c :") 'execute-extended-command)

;; Grep
(evil-leader/set-key
  "*" 'grep-auto-select)

(global-set-key (kbd "C-c *") 'grep-auto-select)

;; Buffers
(evil-leader/set-key
  "bn" 'new-empty-buffer
  "br" 'revert-buffer
  "bk" 'kill-all-buffers
  "bd" 'kill-current-buffer
  "bb" 'ivy-switch-buffer)

(global-set-key (kbd "C-c b n") 'new-empty-buffer)
(global-set-key (kbd "C-c b r") 'revert-buffer)
(global-set-key (kbd "C-c b k") 'kill-all-buffers)
(global-set-key (kbd "C-c b d") 'kill-current-buffer)
(global-set-key (kbd "C-c b b") 'ivy-switch-buffer)

;; Display info
(evil-leader/set-key
  "dp" 'display-current-buffer-path)

(global-set-key (kbd "C-c d p") 'display-current-buffer-path)

;; Window manipulation
(evil-leader/set-key
  "w3" 'split-window-right
  "w2" 'split-window-below
  "w1" 'delete-other-windows
  "w0" 'delete-window
  "wd" 'delete-window
  ;; Windmove
  "wj" 'windmove-down
  "wk" 'windmove-up
  "wh" 'windmove-left
  "wl" 'windmove-right
  ;; Buffer move
  "wJ" 'buf-move-down
  "wK" 'buf-move-up
  "wH" 'buf-move-left
  "wL" 'buf-move-right
  ;; Other window
  "wo" 'ace-window
  ;; Balance windows
  "w+" 'balance-windows)

(global-set-key (kbd "C-c w 3") 'split-window-right)
(global-set-key (kbd "C-c w 2") 'split-window-below)
(global-set-key (kbd "C-c w 1") 'delete-other-windows)
(global-set-key (kbd "C-c w 0") 'delete-window)
(global-set-key (kbd "C-c w d") 'delete-window)
(global-set-key (kbd "C-c w h") 'windmove-left)
(global-set-key (kbd "C-c w l") 'windmove-right)
(global-set-key (kbd "C-c w k") 'windmove-up)
(global-set-key (kbd "C-c w j") 'windmove-down)
(global-set-key (kbd "C-c w H") 'buf-move-left)
(global-set-key (kbd "C-c w L") 'buf-move-right)
(global-set-key (kbd "C-c w K") 'buf-move-up)
(global-set-key (kbd "C-c w J") 'buf-move-down)
(global-set-key (kbd "C-c w o") 'ace-window)
(global-set-key (kbd "C-c w +") 'balance-windows)

;; Help
(evil-leader/set-key
  "hv" 'describe-variable
  "hf" 'describe-function
  "hk" 'describe-key)

;; Hydras
(evil-leader/set-key
  "<RET>z" 'hydra-zoom/body
  "<RET>t" 'hydra-theme/body
  "<RET>c" 'hydra-code/body)

(global-set-key (kbd "C-c <RET> c") 'hydra-code/body)
(global-set-key (kbd "C-c <RET> z") 'hydra-zoom/body)
(global-set-key (kbd "C-c <RET> t") 'hydra-theme/body)

;; Yasnippets
(evil-leader/set-key
  "yi" 'yas-insert-snippet)

;; LSP mode
(evil-leader/set-key
  "lD" 'lsp-find-definition
  "ld" 'lsp-ui-peek-find-definitions
  "lR" 'lsp-find-references
  "lr" 'lsp-ui-peek-find-references)

;; Lisp evaluation
(evil-leader/set-key
  "`e" 'eval-last-sexp
  "`r" 'eval-region)

(global-set-key (kbd "C-c ` e") 'eval-last-sexp)
(global-set-key (kbd "C-c ` r") 'eval-region)

;; Bookmarks
(evil-leader/set-key
  "rm" 'bookmark-set
  "rb" 'bookmark-jump
  "rl" 'bookmark-bmenu-list)

;; Orgmode
(evil-leader/set-key
  "ob" '(lambda() (interactive) (find-file "~/orgmode/bookmarks.org"))
  "ot" '(lambda() (interactive) (find-file "~/orgmode/tasks.org"))
  "os" '(lambda() (interactive) (find-file "~/orgmode/sticky_notes.org"))
  "oo" 'org-open-at-point)

;; Disable this to avoid hitting it accidentally, evil has its own keys for this
(global-unset-key (kbd "C-x C-c"))

(provide 'my-keybindings)
