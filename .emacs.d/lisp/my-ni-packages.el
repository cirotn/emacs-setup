;; NI-Emacs
(require 'ni-emacs/util)
(require 'ni-emacs/browse)
(require 'ni-emacs/directory)
(require 'ni-emacs/grep)
(require 'ni-emacs/insert-code)
(require 'ni-emacs/perforce)
(require 'ni-emacs/component-dirs)
(require 'ni-emacs/package-file)
(require 'ni-emacs/ctags)
(require 'ni-emacs/build)
(require 'ni-emacs/p4login)
(require 'ni-emacs/ni/package-mode)

;; ctags path
(setq ni-ctags-path-to-ectags "~/.emacs.d/lisp/ni-emacs/tools/win32/i386")

(provide 'my-ni-packages)
