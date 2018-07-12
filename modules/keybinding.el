;; Custom keybinding

;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

;; General
(use-package general
  :ensure t
  :init
  :config
  (general-define-key
   :prefix "C-c"
   "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
   "SPC" '(helm-M-x :which-key "M-x")

   "!" '(:which-key "flycheck")
   "$" '(:which-key "correct word")

   "e" '(:which-key "checks")
   "el" '(flycheck-list-errors :which-key "list errors")

   "g" '(:which-key "git")
   "gg" '(magit :which-key "magit")
   "gb" '(magit-blame :which-key "magit blame")
   "gh" '(git-timemachine :which-key "history")
   
   "f" '(:which-key "files")
   "ff" '(helm-find-files :which-key "find file")
   "fk" '(delete-file :which-key "delete file")
   
   "p" '(:which-key "project")
   "pf" '(helm-projectile-find-file)
   ;;"ps" '(helm-projectile-ag :which-key "find occurrences")
   "pp" '(helm-projectile-switch-project)
   "ps" '(:which-key "search")
   "px" '(:which-key "shell")
	  
   
   "b" '(:which-key "buffers")
   "b]" '(next-buffer :which-key "next buffer")
   "b[" '(previous-buffer :which-key "previous buffer")
   "bk" '(kill-buffer :which-key "kill buffer")
   "bb" '(helm-buffers-list :which-key "buffers list")
   "be" '(eval-buffer :which-key "eval buffer")

   "r" 'ranger
   
   "w" '(:which-key "window")
   "w]" '(next-multiframe-window :which-key "next window")
   "w[" '(previous-multiframe-window :which-key "previous window")
   "w/" '(split-window-right :which-key "split right")
   "w-" '(split-window-below :which-key "split bottom")
   "wd" '(delete-window :which-key "delete window")
   ))

(provide 'keybinding)
