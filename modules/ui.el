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

(provide 'ui)
