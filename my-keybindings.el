;; Keymap for buffers
(global-set-key (kbd "C-c b n") 'new-empty-buffer)
(global-set-key (kbd "C-c b k") 'kill-all-buffers)
(global-set-key (kbd "C-c b r") 'revert-buffer)

;; Keymap for compile
(global-set-key (kbd "C-c c") 'compile)

;; Keymaps for python
(global-set-key (kbd "C-c p n") 'python-util-new-empty-buffer)
(global-set-key (kbd "C-c p d") 'python-debug-with-pdb)

;; Keymaps for display info
(global-set-key (kbd "C-c d p") 'display-current-buffer-path)

;; Keymaps for grep
(global-set-key (kbd "C-c g") 'grep-auto-select)

;; Keymaps for changing window appearance
(global-set-key (kbd "C-c w t") 'toggle-truncate-lines)
(global-set-key (kbd "C-c w w") 'toggle-show-trailing-ws)
;; (global-set-key (kbd "C-c w r") 'fci-mode)

;; Keymaps for etags
(global-set-key (kbd "C-c t l") 'etags-select-find-tag-at-point)
(global-set-key (kbd "C-c 4 t l") 'etags-select-find-tag)

;; Redraw display to get around glitch where screen scrambles randomly
(global-set-key (kbd "C-c r") 'redraw-display)

;; Disable this to avoid hitting it accidentally, evil has its own keys for this
(global-unset-key (kbd "C-x C-c"))

(provide 'my-keybindings)
