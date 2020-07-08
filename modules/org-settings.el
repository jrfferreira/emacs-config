(use-package org-gcal
  :ensure t
  :after (org org-agenda org-capture)
  :init
  (setq org-gcal-client-id (getenv "CAL_ID")
      org-gcal-client-secret (getenv "CAL_SECRET")
      org-gcal-file-alist '(("jr8116@gmail.com" .  "~/MEGAsync/org/gcal.org")))
  (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
  (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))
  )

(provide 'org-settings)
