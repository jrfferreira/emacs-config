
;;; python
      
(use-package blacken
  :ensure t
  :after (python)
  :init
  :bind (:map python-mode-map ("C-c C-b" . blacken-buffer)
  :bind (:map python-mode-map ("C-c C-B" . blacken-mode))
  )

(use-package company-jedi
  :ensure t
  :after (company eldoc flycheck python pyenv-mode python-docstring sphinx-doc)
  :init
  (defun my/python-mode-hook ()
    (pyenv-mode)
    (flycheck-mode)
    (eldoc-mode)
    (company-mode)
    (sphinx-doc-mode)
    (python-docstring-mode)
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook #'my/python-mode-hook)
  :bind (:map python-mode-map ("M-." . 'jedi:goto-definition)))

(provide 'lang-python)
;;; python.el ends here
