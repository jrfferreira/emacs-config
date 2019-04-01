;;; Rust mode

(use-package rust-mode
  :ensure t
  :after (flycheck-rust)
  :mode (("\\.rs\\'" . rust-mode))
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  )

(provide 'lang-rust)
