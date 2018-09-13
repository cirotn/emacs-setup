;; FILE  : config.el
;; AUTHOR: Justin Davis (justin.davis@ni.com)
;; DESC  : Contains some basic configuration

(require 'cc-vars)  ;; Required for updating c++-font-lock-extra-types

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Perforce Configuration Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar perforce-port
  "perforce.natinst.com:1666"
  "The location of the \"perforce\" server.  99.9% of the time, it should be \"perforce.natinst.com:1666\".")

(defvar penguin-port
  "penguin.natinst.com:1666"
  "The location of the \"penguin\" server.  99.9% of the time, it should be \"penguin.natinst.com:1666\".")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ Style Configuration Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cc-mode)
(setq c-default-style
      '((java-mode . "java")
        (awk-mode  . "awk")
        (other     . "ni-ss")))

(defvar ni-ss-style
  '( "gnu" ;; Base style
     (c-basic-offset . 3)
     (c-offsets-alist
      (access-label        . -2)
      (substatement-open   . 0)
      (topmost-intro       . 0)
      (case-label          . +)
      (statement-case-open . 0)
      (innamespace         . 0)
      (cpp-define-intro    . c-lineup-cpp-define)
      (member-init-cont    . c-lineup-multi-inher)
      (stream-op           . c-lineup-streamop)
      (arglist-close       . c-lineup-arglist)))
  "The base style for ni-emacs.  Based on Systems Software standards.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ctags Configuration Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar path-to-ectags
  ""
  "The path to the exhuberant ctags executable on your machine.")

(defvar ectags-options
  "-e --recurse=yes --langmap=C++:+.ipp"
  "The command line options used when generating ctags.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add to auto-mode-alist
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist
      (append '(("package$" . package-mode)
                ("\\.mak$" . makefile-mode)
                ("\\.bat$" . bat-mode)
                ("\\.ipp$" . c++-mode)
                ("\\.h$"   . c++-mode)
                ("\\.top$" . lisp-mode)
                ("\\.rb$"  . ruby-mode)
                ) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming Specific Configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (c-add-style "ni-ss" ni-ss-style)

(defun ni-common-programming-hook ()
  (camelCase-mode 1)          ; Enable camelCase word boundary definition
  (setq indent-tabs-mode nil) ; Indent never inserts tab characters
  ; Visit the tags for the buffer if we can
  (ni-visit-tags-for-buffer))

(add-hook 'c-mode-hook
          '(lambda ()
             ;;  Use completing-newline-and-indent instead of newline-and-indent
             (ni-common-programming-hook)
             (local-set-key "\C-j" 'completing-newline-and-indent)
             ;; Use smart-indent and smart-back-indent
             (local-set-key [(tab)] 'smart-indent)
             (local-set-key [(backtab)] 'smart-back-indent)))
(add-hook 'c++-mode-hook
          '(lambda ()
             ;;  Use completing-newline-and-indent instead of newline-and-indent
             (ni-common-programming-hook)
             (local-set-key "\C-j" 'completing-newline-and-indent)
             ;; Use smart-indent and smart-back-indent
             (local-set-key [(tab)] 'smart-indent)
             (local-set-key [(backtab)] 'smart-back-indent)))
(add-hook 'python-mode-hook
          '(lambda ()
             (ni-common-programming-hook)
             (setq python-indent 3)))
(add-hook 'lisp-mode-hook
          'ni-common-programming-hook)
(add-hook 'emacs-lisp-mode-hook
          'ni-common-programming-hook)

;; Add PAL syntax font-lock keywords
(let ((extra-types '("i32" "u32" "i64" "u64" "i8" "u8" "i16" "u16" "f32" "f64" "\\<t[A-Z][a-zA-Z0-9_-]*\\>" "\\<i[A-Z][a-zA-Z0-9_-]*\\>")))
  (setq c++-font-lock-extra-types (append extra-types c++-font-lock-extra-types))
  (setq c-font-lock-extra-types (append extra-types c-font-lock-extra-types)))

;; Default to never inserting tabs
(setq-default indent-tabs-mode nil)
(show-paren-mode t)
(setq perl-indent-level 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'shell-mode-hook
          '(lambda ()
            (pcomplete-shell-setup)
            (setq pcomplete-cycle-cutoff-length 1)
            (compilation-shell-minor-mode 1)
            (local-set-key "\M-s" 'ni-open-new-shell)
            (camelCase-mode t)
            ;; This next line is to work around a bug in the ansi
            ;; color mode stuff.
            (setq comint-last-output-start (set-mark (point-min)))
            (ansi-color-for-comint-mode-on))
          t)

;; Make tab complete enter a backslash instead of a forward slash
(setq comint-completion-addsuffix '("\\" . ""))

;; Run our output filter after commands have run
(add-hook 'comint-output-filter-functions
          'ni-shell-output-filter-function)

(provide 'ni-emacs/ni/config)
