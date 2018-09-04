;; Custom keybinding

;; Ensuring alt as meta
(setq-default mac-option-key-is-meta t
      mac-command-key-is-meta nil
      mac-command-modifier 'super
      mac-option-modifier 'meta
      mac-right-option-modifier nil)

;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1)
  (which-key-setup-side-window-right-bottom))

;; OSX Bindings
(if (eq system-type "darwin")
  (global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
  (global-set-key (kbd "s-v") 'yank)
  (global-set-key (kbd "s-c") 'kill-ring-save)
  (global-set-key (kbd "s-a") 'mark-whole-buffer)
  (global-set-key (kbd "s-f") 'isearch-forward)
  (global-set-key (kbd "s-x") 'kill-region)
  (global-set-key (kbd "s-w") 'delete-window)
  (global-set-key (kbd "s-W") 'delete-frame)
  (global-set-key (kbd "s-n") 'make-frame)
  (global-set-key (kbd "s-z") 'undo-tree-undo)
  (global-set-key (kbd "s-<return>") 'toggle-frame-fullscreen)
  (global-set-key (kbd "s-s")
                  (lambda ()
                    (interactive)
                    (call-interactively (key-binding "\C-x\C-s"))))
  (global-set-key (kbd "s-Z") 'undo-tree-redo)
  )

;; shit+click to extend selected region
(define-key global-map (kbd "<S-mouse-1>") 'mouse-set-point)
(define-key global-map (kbd "<S-down-mouse-1>") 'mouse-save-then-kill)


;; Easily moving between frames
(define-key global-map (kbd "s-]") 'next-multiframe-window)
(define-key global-map (kbd "s-[") 'previous-multiframe-window)

;; Ignoring suspende
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-x C-z") nil)

;; General
(use-package general
  :ensure t
  :config
  (general-define-key
   :prefix "C-c"
   "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
   "SPC" '(helm-M-x :which-key "M-x")

   "!" '(:which-key "flycheck")
   "$" '(:which-key "correct word")

   "d" '(:which-key "helm-dash")
   "dd" '(helm-dash :which-key "search")
   "di" '(helm-dash-install-docset :which-key "install-docset")
   "dD" '(helm-dash-at-point :which-key "search-at-point")

   "e" '(:which-key "checks")
   "el" '(flycheck-list-errors :which-key "list errors")

   "g" '(:which-key "git")
   "gg" '(magit :which-key "magit")
   "gb" '(magit-blame :which-key "magit blame")
   "gh" '(git-timemachine :which-key "history")
   
   "f" '(:which-key "files")
   "ff" '(helm-find-files :which-key "find file")
   "fk" '(delete-file :which-key "delete file")
   "fd" '(utils-delete-file-and-buffer :which-key "delete active file")
   "fR" '(utils-rename-file-and-buffer :which-key "rename active file")
   "fy" '(utils-copy-file-name-to-clipboard :which-key "copy file name")
   "fe" '(:which-key "emacs")
   "fed" '(utils-open-user-init-file :which-key "init file")
   
   "p" '(:which-key "project")
   "ph" '(helm-projectile :which-key "projectile")
   "pf" '(helm-projectile-find-file :which-key "find file")
   "pp" '(helm-projectile-switch-project :which-key "switch project")
   "ps" '(helm-projectile-ag :which-key "search")
   "px" '(:which-key "shell")
	  
   "b" '(:which-key "buffers")
   "b]" '(next-buffer :which-key "next buffer")
   "b[" '(previous-buffer :which-key "previous buffer")
   "bk" '(kill-buffer :which-key "kill buffer")
   "bb" '(helm-buffers-list :which-key "buffers list")
   "bd" '(kill-this-buffer :which-key "kill active buffer")
   "be" '(eval-buffer :which-key "eval buffer")
   "bN" '(utils-create-scratch-buffer :which-key "new buffer")
   "bi" '(helm-imenu :which-key "helm-imenu")
   "bR" '(query-replace :which-key "replace in buffer (M-%)")
   
   "r" 'ranger

   "t" '(:which-key "term")
   "tt" '(multi-term :which-key "mult-term")
   "t]" '(multi-term-next :which-key "next term")
   "t[" '(multi-term-prev :which-key "prev term")
   
   "w" '(:which-key "window")
   "w]" '(next-multiframe-window :which-key "next window")
   "w[" '(previous-multiframe-window :which-key "previous window")
   "w/" '(split-window-right :which-key "split right")
   "w-" '(split-window-below :which-key "split bottom")
   "wd" '(delete-window :which-key "delete window")
   ))

(provide 'keybinding)
