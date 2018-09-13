;; Map C-x C-c to kell other buffers
(global-set-key (kbd "C-x C-c") 'kill-all-buffers)

;; Keymap for new buffer
(global-set-key (kbd "C-x C-n") 'new-empty-buffer)

;; Keymap for windmove
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c k") 'windmove-up)
(global-set-key (kbd "C-c j") 'windmove-down)

;; Keymap for buffer move
(global-set-key (kbd "C-c H") 'buf-move-left)
(global-set-key (kbd "C-c L") 'buf-move-right)
(global-set-key (kbd "C-c K") 'buf-move-up)
(global-set-key (kbd "C-c J") 'buf-move-down)

;; Keymap for compile
(global-set-key (kbd "C-c c") 'ni-build)
(global-set-key (kbd "C-c 4 c") 'ni-build-with-selection)

;; Keymaps for python
(global-set-key (kbd "C-c p n") 'python-util-new-empty-buffer)
(global-set-key (kbd "C-c p d") 'python-debug-with-pdb)

;; Keymaps for opening files
(global-set-key (kbd "C-c o p") 'ni-component-dirs-open-package)
(global-set-key (kbd "C-c 4 o p") 'ni-component-dirs-open-package-other-window)
(global-set-key (kbd "C-c o d") 'ni-package-file-open-dependency-dir)
(global-set-key (kbd "C-c 4 o d") 'ni-package-file-open-dependency-dir-other-window)
(global-set-key (kbd "C-c o i") 'ni-package-file-open-include)
(global-set-key (kbd "C-c 4 o i") 'ni-package-file-open-include-other-window)
(global-set-key (kbd "C-c o m") 'ni-component-dirs-open-makefile)
(global-set-key (kbd "C-c o e") 'ni-directory-open-file-in-explorer)
(global-set-key (kbd "C-c o n")  'ni-directory-visit-next-file-with-base-name)
(global-set-key (kbd "C-c 4 o n") 'ni-directory-visit-next-file-with-base-name-other-window)
(global-set-key (kbd "C-c o f") 'ni-component-dirs-file-open-source-file)
(global-set-key (kbd "C-c 4 o f") 'ni-component-dirs-file-open-source-file-other-window)

;; Keymaps for display info
(global-set-key (kbd "C-c d p") 'ni-util-display-current-buffer-path)

;; Keymaps for inserting code
(global-set-key (kbd "C-c i h") 'ni-insert-code-file-header)
(global-set-key (kbd "C-c i i") 'ni-insert-code-include-guards)

;; Keymaps for grep NI project
(global-set-key (kbd "C-c n g") 'ni-grep-interactive)

;; Keymaps for git
(global-set-key (kbd "C-c g g") 'grep-git-repository)

;; Keymaps for display info
(global-set-key (kbd "C-c f w") 'ni-util-set-file-writable)

;; Browser
(global-set-key (kbd "C-c b g") 'ni-browse-google)
(global-set-key (kbd "C-c b c") 'ni-browse-cplusplus)
(global-set-key (kbd "C-c b m") 'ni-browse-msdn)

;; Keymaps for changing window appearance
(global-set-key (kbd "C-c w t") 'toggle-truncate-lines)
(global-set-key (kbd "C-c w s") (lambda() (interactive) (w32-send-sys-command #xf130))) ;; start menu
(global-set-key (kbd "C-c w m") (lambda() (interactive) (w32-send-sys-command #xf030))) ;; maximize
(global-set-key (kbd "C-c w n") (lambda() (interactive) (w32-send-sys-command #xf020))) ;; minimize
(global-set-key (kbd "C-c w w") 'ni-util-toggle-show-trailing-ws)

;; Keymaps for etags
(global-set-key (kbd "C-c t v") 'ni-ctags-visit-tags-table)
(global-set-key (kbd "C-c t c") 'ni-ctags-gen)
(global-set-key (kbd "C-c t l") 'etags-select-find-tag-at-point)
(global-set-key (kbd "C-c 4 t l") 'etags-select-find-tag)

;; Redraw display to get around glitch where screen scrambles randomly
(global-set-key (kbd "C-c r") 'redraw-display)

(provide 'my-keybindings)
