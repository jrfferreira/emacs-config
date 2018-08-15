;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(global-hl-line-mode t)
(global-display-line-numbers-mode t)
(setq inhibit-default-init t)
(setq display-line-numbers "%4d ")
(setq inhibit-startup-screen t)

;; Initial screen
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  :init
  ;; Set the title
  (setq dashboard-banner-logo-title "...and here we are again.")
  (setq dashboard-startup-banner (expand-file-name "./img/zombie.png" user-emacs-directory))
  ;; Items
  (setq dashboard-items '((recents  . 5)
                        (projects . 5)
                        (agenda . 5))))


;; prefer vertical split
(setq split-height-threshold nil)
(setq split-width-threshold 150)


;; Font and sizes
(add-to-list 'default-frame-alist '(font . "Fira Code Retina 16"))
(add-to-list 'default-frame-alist '(height . 30))
(add-to-list 'default-frame-alist '(width . 90))


;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon nil)
(setq frame-title-format "%b")

(use-package ace-window
  :ensure t
  :init
  (setq aw-scope 'frame)
  :config
  (ace-window-display-mode t))

;; Theme
(use-package doom-themes
  :ensure t
  :init
  (setq doom-themes-enable-bold nil
        doom-themes-enable-italic nil)
  :config
  (load-theme 'doom-one t))

(use-package doom-modeline
      :ensure t
      :defer t
      :hook (after-init . doom-modeline-init))

(use-package rainbow-mode
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-mode))

(provide 'ui)
