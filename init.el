;;; Package configs
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
(defvar default-shell "/bin/zsh")
(setenv "SHELL" default-shell)
(setq-default explicit-shell-file-name default-shell)

;; Ensuring user shell vars
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :init
  (setq exec-path-from-shell-shell-name default-shell)
  :config
  (exec-path-from-shell-initialize))

;; Avoid lock files
(setq create-lockfiles nil)

;; Disable backup files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq create-lockfiles nil)

;; [Modules]
(load-file (expand-file-name "./modules/core.el" user-emacs-directory))
(load-file (expand-file-name "./modules/ui.el" user-emacs-directory))
(load-file (expand-file-name "./modules/keybinding.el" user-emacs-directory))

;; [Languages]
(load-file (expand-file-name "./lang/javascript.el" user-emacs-directory))
(load-file (expand-file-name "./lang/yaml.el" user-emacs-directory))

;; Auto generated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-javascript-flow-args nil)
 '(package-selected-packages
   (quote
    (git-timemachine doom-modeline exec-path-from-shell yaml-mode company-tide flycheck-flow company-flow rjsx-mode indent-guide git-gutter git-gutter-fringe+ helm-ag helm-projectile toggle-quotes doom-themes use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
