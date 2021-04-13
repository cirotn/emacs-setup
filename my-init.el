;;(toggle-debug-on-error)

;; Setup package manager and use-package
(setq package-archives '(("org"       . "https://orgmode.org/elpa/")
                         ("gnu"       . "https://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(add-to-list 'load-path "~/emacs-setup")
(require 'my-packages)
(require 'my-config)
(require 'util)
(require 'git-util)
(require 'grep-util)
(require 'my-hydras)
(require 'my-keybindings)
(require 'my-preferences)

(put 'dired-find-alternate-file 'disabled nil)
