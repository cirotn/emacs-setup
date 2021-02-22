;; Find file
(evil-leader/set-key
  "<SPC>" 'projectile-find-file)

;; Execute
(evil-leader/set-key
  ":" 'execute-extended-command)

;; Grep
(evil-leader/set-key
  "*" 'grep-auto-select)

;; Buffers
(evil-leader/set-key
  "bn" 'new-empty-buffer
  "br" 'revert-buffer
  "bk" 'kill-all-buffers
  "bd" 'kill-current-buffer
  "bb" 'ivy-switch-buffer)

;; Display info
(evil-leader/set-key
  "dp" 'display-current-buffer-path)

;; Window manipulation
(evil-leader/set-key
  "w3" 'split-window-right
  "w2" 'split-window-below
  "w1" 'delete-other-windows
  "w0" 'delete-window
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

(evil-leader/set-key
  "`e" 'eval-last-sexp
  "`r" 'eval-region)

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
