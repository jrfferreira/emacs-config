;;; Javascript

;; RJSX/ELINT

(use-package tide
  :ensure t
  :after (company flycheck)
  :init
  (setq-default
   ;tide-tsserver-executable "/usr/local/bin/tsserver"
   tide-format-options '(:indentSize 2 :indentStyle 2 :tabSize 2 :ConvertTabsToSpaces t)
   ))

;; In case of no result of eslint, check the config:
;; $  eslint --print-config .
;; Tide / JS
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (eldoc-mode +1)
  (company-mode +1)
  ;; (tide-hl-identifier-mode +1)
  (setq
   ;; flycheck-check-syntax-automatically '(save mode-enabled)
   tide-format-options '(:indentSize 2 :indentStyle 2 :tabSize 2 :ConvertTabsToSpaces t)
   tide-tsserver-executable "/usr/local/bin/tsserver"
   tide-completion-detailed t
     tide-always-show-documentation t
     )
    )

(defun init-flycheck ()
  (interactive)
  (use-package flycheck-flow
    :after (tide company flycheck)
    :init
    (progn
      (setq-default flycheck-disabled-checkers '(javascript-flow-coverage))
      (setq flycheck-display-errors-delay 0.5)
      (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change))
      (flycheck-add-mode 'javascript-flow 'rjsx-mode)
      (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
      (flycheck-add-next-checker 'javascript-flow 'javascript-eslint)))
    (with-eval-after-load 'flycheck
      (push 'javascript-jshint flycheck-disabled-checkers)
      (push 'json-jsonlint flycheck-disabled-checkers)))

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
    :after (company tide)
    :defer t
    :commands company-flow
    :init
    (eval-after-load 'company
      (add-to-list 'company-backends 'company-flow))
    (add-to-list 'company-flow-modes 'rjsx-mode)))


(use-package rjsx-mode
  :after (tide)
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
  (add-hook 'rjsx-mode-hook #'setup-tide-mode)
  (add-hook 'rjsx-mode-hook #'init-company-flow)
  (add-hook 'rjsx-mode-hook #'init-flycheck)
  )

(use-package prettier-js
  :after (rjsx-mode)
  :ensure t
  :init
  (add-hook 'rjsx-mode-hook #'prettier-js-mode))

