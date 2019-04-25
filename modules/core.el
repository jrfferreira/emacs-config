;;; Core modules

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

(use-package restclient
  :ensure t)

(use-package multi-term
  :ensure t)

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
  (helm-mode 1)
  :bind (("C-x C-f" . #'helm-find-files)
	 ("C-x b" . #'helm-buffers-list)
	 ("M-x" . #'helm-M-x)
	 :map helm-map
	 ("TAB" . #'helm-execute-persistent-action)
	 ("<tab>" . #'helm-execute-persistent-action)
	 ("C-z" . #'helm-select-action)
	 ))

(use-package helm-projectile
  :ensure t
  :after (helm)
  :init
  (setq projectile-completion-system 'helm
	helm-projectile-fuzzy-match t
	projectile-indexing-method 'alien
	projectile-enable-caching t
	projectile-switch-project-action 'helm-projectile)
  :config
  (projectile-mode)
  (helm-projectile-on))

;; (use-package helm-dash
;;   :ensure t
;;   :after (helm))

(use-package ranger
  :ensure t
  :init
  (setq ranger-show-hidden t))

(use-package flycheck
  :ensure t
  :init
  :config
  (global-flycheck-mode))

;; Company
(use-package company
  :ensure t
  :bind(:map company-active-map
	     ("RET" . #'company-complete-selection))
  :init
   ;; Delete consecutive dupes from company in case they differ by annotation only
  ;; https://github.com/company-mode/company-mode/issues/528
  (with-eval-after-load 'company
    (add-to-list 'company-transformers 'delete-consecutive-dups t))
  (setq company-idle-delay 0)
  (setq company-tooltip-align-annotations t)
  ;;(setq company-minimum-prefix-length 2)
  :config
  (global-company-mode t)
  ;; Use tab key to cycle through suggestions.
  ;; ('tng' means 'tab and go')
  (company-tng-configure-default))

(use-package projectile
  :ensure t
  :init
  (setq projectile-require-project-root nil)
  :config
  (projectile-mode 1))

(use-package magit
  :ensure t
  :init)

(use-package git-timemachine
  :ensure t)

(use-package git-gutter
  :ensure t
  :init
  :config
  (global-git-gutter-mode t))

(use-package git-messenger
  :ensure t
  :after (magit magit-popup)
  :init
  )

(use-package indent-guide
  :ensure t
  :init
  (setq indent-guide-char " ")
  :config
  (indent-guide-global-mode)
  (set-face-background 'indent-guide-face "black"))


(use-package multiple-cursors
  :ensure t
  :init
  :config
  (global-set-key (kbd "C->") #'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") #'mc/mark-previous-like-this)
  (global-set-key (kbd "C-M-<") #'mc/mark-all-like-this)
  (global-set-key (kbd "C-M->") #'mc/edit-lines)
  (defun mc/mouse-quit ()
    "Deactivate mark if there are any active, and exit multiple-cursors-mode."
    (interactive)
    (if (use-region-p)
        (deactivate-mark))
    (multiple-cursors-mode 0))
  (global-set-key (kbd "<down-mouse-1>") #'mc/mouse-quit)
  (global-unset-key (kbd "C-S-<down-mouse-1>"))
  (global-set-key (kbd "C-S-<mouse-1>") #'mc/add-cursor-on-click)
  )

;; Highlight similar text
(use-package highlight-thing
  :ensure t
  :init
  (setq highlight-thing-case-sensitive-p t)
  (setq highlight-thing-what-thing 'word)
  (setq highlight-thing-exclude-thing-under-point t)
  :config
  (global-highlight-thing-mode))


(use-package smartparens
  :ensure t
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-mode))


(use-package toggle-quotes
  :ensure t
  :bind
  ("C-'" . 'toggle-quotes))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))


;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;; replace selected region
(delete-selection-mode 1)

(provide 'core)
