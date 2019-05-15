;;; Rust mode

(use-package rust-mode
  :ensure t
  :after (flycheck-rust)
  :mode (("\\.rs\\'" . rust-mode))
  
  :bind (:map rust-mode-map
	      ("C-c C-f" . rust-format-buffer)
	      ("C-c C-r" . rust-run)
	      ("C-c C-c" . rust-compile))
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  )

(provide 'lang-rust)
