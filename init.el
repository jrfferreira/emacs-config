;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(global-hl-line-mode t)
(global-linum-mode t)
(setq-default linum-format "%4d ")

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
   
   "g" '(:which-key "magit")
   "gs" '(magit-status :which-key "magit status")
   "gb" '(magit-status :which-key "magit blame")
   
   "f" '(:which-key "files")
   "ff"  '(helm-find-files :which-key "find files")
   
   "p" '(:which-key "project")
   "ps"  '(helm-projectile-find-file :which-key "find files")
   "pf"  '(helm-projectile-ag :which-key "find occurrences")
   
   "b" '(:which-key "buffers")
   "bk"  '(kill-buffer :which-key "kill buffer")
   "bb"  '(helm-buffers-list :which-key "buffers list")
   
   "w" '(:which-key "window")
   "w/"  '(split-window-right :which-key "split right")
   "w-"  '(split-window-below :which-key "split bottom")
   "wd"  '(delete-window :which-key "delete window")
   ))


;; Helm
(use-package helm
  :ensure t
  :init
  (setq helm-M-x-fuzzy-match t
  helm-mode-fuzzy-match t
  helm-buffers-fuzzy-matching t
  helm-recentf-fuzzy-match t
  helm-locate-fuzzy-match t
  helm-semantic-fuzzy-match t
  helm-imenu-fuzzy-match t
  helm-completion-in-region-fuzzy-match t
  helm-candidate-number-list 150
  helm-split-window-in-side-p t
  helm-move-to-line-cycle-in-source t
  helm-echo-input-in-header-line t
  helm-autoresize-max-height 0
  helm-autoresize-min-height 20)
  :config
  (helm-mode 1))

(use-package helm-projectile
  :config
  (helm-projectile-on))


;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))


;; [Frameworks]

(use-package company
  :ensure t
  :init
   ;; Delete consecutive dupes from company in case they differ by annotation only
  ;; https://github.com/company-mode/company-mode/issues/528
  (with-eval-after-load 'company
    (add-to-list 'company-transformers 'delete-consecutive-dups t))
  (setq company-idle-delay 0.1)
  (setq company-tooltip-align-annotations t)
  :config
  (global-company-mode t))


(use-package projectile
  :ensure t
  :init
  (setq projectile-require-project-root nil)
  :config
  (projectile-mode 1))


(use-package magit
  :ensure t
  :init)


(use-package flycheck
  :ensure t
  :init
  :config
  (global-flycheck-mode))


(use-package multiple-cursors
  :ensure t
  :init
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-M-<") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-M->") 'mc/edit-lines)
  (defun mc/mouse-quit ()
    "Deactivate mark if there are any active, and exit multiple-cursors-mode."
    (interactive)
    (if (use-region-p)
        (deactivate-mark))
    (multiple-cursors-mode 0))
  (global-set-key (kbd "<down-mouse-1>") #'mc/mouse-quit)
  (global-unset-key (kbd "C-S-<down-mouse-1>"))
  (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
  )


(use-package highlight-thing
  :ensure t
  :init
  (setq highlight-thing-case-sensitive-p t)
  (setq highlight-thing-what-thing 'word)
  (setq highlight-thing-exclude-thing-under-point t)
  :config
  (global-highlight-thing-mode))


(use-package toggle-quotes
  :ensure t
  :init
  (global-set-key (kbd "C-'") 'toggle-quotes))

;; [General changes]

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)


;; Disable backup files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq create-lockfiles nil)


;; Auto generated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (helm-ag helm-projectile toggle-quotes doom-themes use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
