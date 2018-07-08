;; Javascript


  (defun init-flycheck ()
    (interactive)
    (use-package flycheck-flow
      :config
      (progn
        (setq-default flycheck-disabled-checkers '(javascript-flow-coverage))
        (setq flycheck-display-errors-delay 0.5)
        (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change))
        (flycheck-add-mode 'javascript-flow 'rjsx-mode)
        (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
        (flycheck-add-next-checker 'javascript-flow 'javascript-eslint)))
    (with-eval-after-load 'flycheck
      (push 'javascript-jshint flycheck-disabled-checkers)
      (push 'json-jsonlint flycheck-disabled-checkers))

    (spacemacs/enable-flycheck 'rjsx-mode))

  ;; flow-company
  (defun set-flow-executable ()
    (interactive)
    (defvar executable "/usr/local/bin/flow")
    (setq-local company-flow-executable executable)
    ;; These are not necessary for this package, but a good idea if you use
    ;; these other packages
    (setq-local flow-minor-default-binary executable)
    (setq-local flycheck-javascript-flow-executable executable))

  (defun init-company-flow ()
    (interactive)
    (use-package company-flow
      :defer t
      :commands company-flow
      :init
      (spacemacs|add-company-backends
        :backends company-flow
        :modes js2-mode rjsx-mode)
      :config
      (add-to-list 'company-flow-modes 'rjsx-mode)))


(use-package rjsx-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode)))
