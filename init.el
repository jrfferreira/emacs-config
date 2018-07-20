;;; Package configs

(defvar default-shell "/bin/zsh")
(defvar default-shell-env-file "~/.zshenv")


(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; [General changes]

;; Defining ZSH as default shell
(setenv "SHELL" default-shell)
(setq-default explicit-shell-file-name default-shell)

;; Ensuring user shell vars
(let ((path (shell-command-to-string (concat ". " default-shell-env-file "; echo -n $PATH"))))
  (setenv "PATH" path)
  (setq exec-path
        (append
         (split-string-and-unquote path ":")
         exec-path)))


;; Avoid lock files
(setq create-lockfiles nil)

;; Disable backup files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq create-lockfiles nil)

;; [Modules]
(add-to-list 'load-path (concat user-emacs-directory "modules"))
(require 'utils)
(require 'core)
(require 'ui)
(require 'keybinding)

;; [Languages]
(add-to-list 'load-path (concat user-emacs-directory "lang"))

(require 'lang-javascript)
(require 'lang-yaml)
(require 'lang-python)

;; Auto generated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-javascript-flow-args nil)
 '(package-selected-packages
   (quote
    (multi-term rainbow-mode beacon blacken dashboard restclient git-timemachine doom-modeline exec-path-from-shell yaml-mode company-tide flycheck-flow company-flow rjsx-mode indent-guide git-gutter helm-ag helm-projectile toggle-quotes doom-themes use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
