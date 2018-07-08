;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(global-hl-line-mode t)
(global-display-line-numbers-mode t)
(setq display-line-numbers "%4d ")

;; prefer vertical split
(setq split-height-threshold nil)
(setq split-width-threshold 150)

;; Font and sizes
(add-to-list 'default-frame-alist '(font . "Fira Code Retina 16"))
(add-to-list 'default-frame-alist '(height . 30))
(add-to-list 'default-frame-alist '(width . 90))

;; Package configs
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


;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)


;; Theme
(use-package doom-themes
  :ensure t
  :init
  (setq doom-themes-enable-bold nil
        doom-themes-enable-italic nil)
  :config
  (load-theme 'doom-one t))


;; Power line
(use-package powerline
  :ensure t
  :init
  (setq powerline-default-separator nil)
  :config
  (powerline-default-theme))


;; Custom keybinding
(use-package general
  :ensure t
  :init
  :config
  (general-define-key
   :prefix "C-,"
   "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
   "SPC" '(helm-M-x :which-key "M-x")

   "e" '(:which-key "checks")
   "el" '(flycheck-list-errors :which-key "list errors")
   
   "g" '(:which-key "magit")
   "gs" '(magit-status :which-key "magit status")
   "gb" '(magit-blame :which-key "magit blame")
   
   "f" '(:which-key "files")
   "ff" '(helm-find-files :which-key "find file")
   "fk" '(delete-file :which-key "delete file")
   "fs" '(helm-ag :which-key "find occurrences")
   
   "p" '(:which-key "project")
   "ps" '(helm-projectile-switch-project :which-key "select project")
   "pf" '(helm-projectile-find-file :which-key "find files")
   "ps" '(helm-projectile-ag :which-key "find occurrences")
   
   "b" '(:which-key "buffers")
   "bk" '(kill-buffer :which-key "kill buffer")
   "bb" '(helm-buffers-list :which-key "buffers list")
   "be" '(eval-buffer :which-key "eval buffer")
   
   "w" '(:which-key "window")
   "w/" '(split-window-right :which-key "split right")
   "w-" '(split-window-below :which-key "split bottom")
   "wd" '(delete-window :which-key "delete window")
   ))




;; [General changes]

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;; prefer vertical split
(setq split-height-threshold nil)
(setq split-width-threshold 150)

;; Avoid lock files
(setq create-lockfiles nil)

;; Disable backup files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq create-lockfiles nil)

(load-file "modules/core.el")
(load-file "lang/javascript.el")
		   
;; Auto generated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (rjsx-mode indent-guide git-gutter git-gutter-fringe+ helm-ag helm-projectile toggle-quotes doom-themes use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
