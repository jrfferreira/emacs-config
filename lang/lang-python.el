
;;; python
      
(use-package blacken
  :ensure t
  :after (python)
  :init
  :bind (:map python-mode-map ("C-c C-b" . blacken-buffer))
  )

(use-package company-jedi
  :ensure t
  :after (company python)
  :init
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'my/python-mode-hook)
  :bind (:map python-mode-map ("M-." . 'jedi:goto-definition)))
  

(provide 'lang-python)
;;; python.el ends here
