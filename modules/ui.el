;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(global-hl-line-mode t)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq inhibit-default-init t)
;;(setq linum-format "%4d")
(setq-default left-fringe-width  0)
(setq-default right-fringe-width 10)

(setq inhibit-startup-screen t)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(setq whitespace-style '(face trailing spaces tabs newline tab-mark empty lines-tail))
(setq whitespace-line-column 120)
(add-hook 'prog-mode-hook 'whitespace-mode)

;; Initial screen
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq page-break-lines-char ?\s)
  :init
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-set-init-info t)
  (setq dashboard-center-content t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-banner-logo-title "\"He that breaks a thing to find out what it is has left the path of wisdom\"")
  (setq dashboard-startup-banner (expand-file-name "./img/zombie.png" user-emacs-directory))
  ;; Items
  (setq dashboard-items '((recents  . 10)
                        (projects . 5)
                        (bookmarks . 5))))


;; prefer vertical split
(setq split-height-threshold nil)
(setq split-width-threshold 150)

;; Font and sizes
(add-to-list 'default-frame-alist '(font . "Fira Code Retina 16"))
(add-to-list 'default-frame-alist '(height . 30))
(add-to-list 'default-frame-alist '(wheight . 'normal))
(add-to-list 'default-frame-alist '(width . 90))

;; Disabling bold faces
(mapc
   (lambda (face)
     (set-face-attribute face nil :weight 'normal))
   (face-list))

;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon nil)
(setq frame-title-format
    '(""
      "%b"
      (:eval
       (let ((project-name (projectile-project-name)))
         (unless (string= "-" project-name)
           (format " :: %s" project-name))))))

;; Theme
(use-package doom-themes
  :ensure t
  :init
  (setq doom-themes-enable-bold nil
        doom-themes-enable-italic nil)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  :config
  ;; used to be doom-wilmersdorf
  (load-theme 'doom-palenight t))

(use-package doom-modeline
      :ensure t
      :defer t
      :hook (after-init . doom-modeline-mode)
      :init
      ;; How tall the mode-line should be (only respected in GUI Emacs).
      ;;(setq doom-modeline-height 70)

      ;; How wide the mode-line bar should be (only respected in GUI Emacs).
      (setq doom-modeline-bar-width 4)

      ;; The limit of the window width.
      ;; If `window-width' is smaller than the limit, some information won't be displayed.
      (setq doom-modeline-window-width-limit fill-column)

      ;; Determines the style used by `doom-modeline-buffer-file-name'.
      ;;
      ;; Given ~/Projects/FOSS/emacs/lisp/comint.el
      ;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
      ;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
      ;;   truncate-with-project => emacs/l/comint.el
      ;;   truncate-except-project => ~/P/F/emacs/l/comint.el
      ;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
      ;;   truncate-all => ~/P/F/e/l/comint.el
      ;;   relative-from-project => emacs/lisp/comint.el
      ;;   relative-to-project => lisp/comint.el
      ;;   file-name => comint.el
      ;;   buffer-name => comint.el<2> (uniquify buffer name)
      ;;
      ;; If you are expereicing the laggy issue, especially while editing remote files
      ;; with tramp, please try `file-name' style.
      ;; Please refer to https://github.com/bbatsov/projectile/issues/657.
      (setq doom-modeline-buffer-file-name-style 'relative-from-project)

      ;; Whether display icons in mode-line or not.
      (setq doom-modeline-icon t)

      ;; Whether display the icon for major mode. It respects `doom-modeline-icon'.
      (setq doom-modeline-major-mode-icon t)

      ;; Whether display color icons for `major-mode'. It respects
      ;; `doom-modeline-icon' and `all-the-icons-color-icons'.
      (setq doom-modeline-major-mode-color-icon t)

      ;; Whether display icons for buffer states. It respects `doom-modeline-icon'.
      (setq doom-modeline-buffer-state-icon t)

      ;; Whether display buffer modification icon. It respects `doom-modeline-icon'
      ;; and `doom-modeline-buffer-state-icon'.
      (setq doom-modeline-buffer-modification-icon t)

      ;; Whether display minor modes in mode-line or not.
      (setq doom-modeline-minor-modes nil)

      ;; If non-nil, a word count will be added to the selection-info modeline segment.
      (setq doom-modeline-enable-word-count nil)

      ;; Whether display buffer encoding.
      (setq doom-modeline-buffer-encoding t)

      ;; Whether display indentation information.
      (setq doom-modeline-indent-info t)

      ;; If non-nil, only display one number for checker information if applicable.
      (setq doom-modeline-checker-simple-format nil)
 
      ;; The maximum displayed length of the branch name of version control.
      (setq doom-modeline-vcs-max-length 12)

      ;; Whether display perspective name or not. Non-nil to display in mode-line.
      (setq doom-modeline-persp-name t)

      ;; Whether display `lsp' state or not. Non-nil to display in mode-line.
      (setq doom-modeline-lsp t)

      ;; Whether display github notifications or not. Requires `ghub` package.
      (setq doom-modeline-github nil)

      ;; The interval of checking github.
      (setq doom-modeline-github-interval (* 30 60))

      ;; Whether display environment version or not
      (setq doom-modeline-env-version t)
 
      ;; Or for individual languages
      (setq doom-modeline-env-enable-python t)
      (setq doom-modeline-env-enable-ruby t)
      (setq doom-modeline-env-enable-perl t)
      (setq doom-modeline-env-enable-go t)
      (setq doom-modeline-env-enable-elixir t)
      (setq doom-modeline-env-enable-rust t)

      ;; Change the executables to use for the language version string
      (setq doom-modeline-env-python-executable "python")
      (setq doom-modeline-env-ruby-executable "ruby")
      (setq doom-modeline-env-perl-executable "perl")
      (setq doom-modeline-env-go-executable "go")
      (setq doom-modeline-env-elixir-executable "iex")
      (setq doom-modeline-env-rust-executable "rustc")
)

(use-package solaire-mode
  :ensure t
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))

(use-package rainbow-mode
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-mode))


;; Highlight similar text
(use-package highlight-thing
  :ensure t
  :init
  (setq highlight-thing-case-sensitive-p t)
  (setq highlight-thing-what-thing 'word)
  (setq highlight-thing-exclude-thing-under-point t)
  (setq highlight-thing-all-visible-buffers-p t)
  :config
  (custom-set-faces
   '(highlight-thing ((t (:inherit lazy-highlight))))
   )
  (global-highlight-thing-mode))

(provide 'ui)
