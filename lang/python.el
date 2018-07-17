;;; Python mode

(use-package blacken
  :ensure t
  :init
  ;; (add-hook 'python-mode-hook 'blacken-mode)
  :bind (:map python-mode-map
	      ("C-c C-b" . blacken-buffer))
  )

(provide 'python)
