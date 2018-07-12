;;; Javascript --- configuration for Javascript language with Flycheck and Flow

;; RJSX/ELINT

(use-package flow-minor-mode
  :ensure t)


(defun init-flycheck ()
  (interactive)
  (use-package flycheck-flow
    :after (flycheck flow-minor-mode)
    :config
    (flycheck-add-next-checker 'javascript-flow 'javascript-eslint)
    :init
    (progn
      (setq-default flycheck-disabled-checkers '(javascript-flow-coverage))
      (setq flycheck-display-errors-delay 0.5)
      (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change)))
    (with-eval-after-load 'flycheck
      (push 'javascript-jshint flycheck-disabled-checkers)
      (push 'json-jsonlint flycheck-disabled-checkers))))

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
    :after (company flow-minor-mode)
    :defer t
    :commands company-flow
    :init
     (with-eval-after-load 'company
       (add-to-list 'company-backends 'company-flow))
    ))


(use-package prettier-js
  :ensure t)

(use-package rjsx-mode
  :after (company flycheck prettier-js)
  :ensure t
  :init
  (setq
   js2-mode-show-strict-warnings nil
   js2-mode-show-parse-errors nil
   js-indent-level 2
   js2-basic-offset 2
   js2-strict-trailing-comma-warning nil
   js2-strict-missing-semi-warning nil)

  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
  (add-hook 'rjsx-mode-hook #'set-flow-executable)
  (add-hook 'rjsx-mode-hook #'init-company-flow)
  (add-hook 'rjsx-mode-hook #'init-flycheck)
  (add-hook 'rjsx-mode-hook #'prettier-js-mode))


(provide 'javascript)
;;; javascript.el ends here
