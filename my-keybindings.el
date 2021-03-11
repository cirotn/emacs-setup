;; Find file
(evil-leader/set-key
  "<SPC>" 'projectile-find-file)

(global-set-key (kbd "C-c <SPC>") 'projectile-find-file)

;; Execute
(evil-leader/set-key
  ":" 'execute-extended-command)

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
  "wL" 'buf-move-right)

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

;; Lisp evaluation
(evil-leader/set-key
  "`e" 'eval-last-sexp
  "`r" 'eval-region)

(global-set-key (kbd "C-c ` e") 'eval-last-sexp)
(global-set-key (kbd "C-c ` r") 'eval-region)

;;  'toggle-truncate-lines
;;  'toggle-show-trailing-ws

;; Keymaps for python
;;(global-set-key (kbd "C-c p n") 'python-util-new-empty-buffer)
;;(global-set-key (kbd "C-c p d") 'python-debug-with-pdb)

;; Keymaps for etags
;;(global-set-key (kbd "C-c t l") 'etags-select-find-tag-at-point)
;;(global-set-key (kbd "C-c 4 t l") 'etags-select-find-tag)

;; Disable this to avoid hitting it accidentally, evil has its own keys for this
(global-unset-key (kbd "C-x C-c"))

(provide 'my-keybindings)
