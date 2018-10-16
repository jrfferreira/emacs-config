;;; YAML mode

(use-package yaml-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(provide 'lang-yaml)
