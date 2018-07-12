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
   "pf" '(helm-projectile-find-file :which-key "find files")
   "ps" '(helm-projectile-ag :which-key "find occurrences")
   "pl" '(helm-projectile-switch-project :which-key "choose project")
   
   "b" '(:which-key "buffers")
   "bk" '(kill-buffer :which-key "kill buffer")
   "bb" '(helm-buffers-list :which-key "buffers list")
   "be" '(eval-buffer :which-key "eval buffer")

   "a" '(:which-key "apps")
   "ar" '(ranger :which-key "ranger")
   
   "w" '(:which-key "window")
   "w/" '(split-window-right :which-key "split right")
   "w-" '(split-window-below :which-key "split bottom")
   "wd" '(delete-window :which-key "delete window")
   ))

(provide 'keybinding)
