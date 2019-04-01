;;; Javascript --- configuration for Javascript language with Flycheck and Flow

;; Code: Define javascript mode configs

(defvar where-bin (if (eq system-type "darwin") "/usr/local/bin/" "/usr/bin/"))

;; RJSX/ELINT
(use-package flow-minor-mode
  :ensure t)

(use-package tide
  :ensure t
  :after (company flycheck))

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
   tide-tsserver-executable (concat where-bin "tsserver")
   tide-completion-detailed t
     tide-always-show-documentation t
     )
    )


(defun init-flycheck ()
  (interactive)
  (use-package flycheck-flow
    :ensure t
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
  (defvar executable (concat where-bin "flow"))
  (setq-local company-flow-executable executable)
  ;; These are not necessary for this package, but a good idea if you use
  ;; these other packages
  (setq-local flow-minor-default-binary executable)
  (setq-local flycheck-javascript-flow-executable executable))

(defun init-company-flow ()
  (interactive)
  (use-package company-flow
    :ensure t
    :after (company flow-minor-mode)
    :defer t
    :commands company-flow
    :init
     (with-eval-after-load 'company
       (add-to-list 'company-backends 'company-flow))
     ))

(defun init-js-indent-line ()
  (interactive)
  (setq-local indent-line-function 'js-jsx-indent-line))


(use-package prettier-js
  :ensure t)

(use-package rjsx-mode
  :after (company flycheck tide prettier-js)
  :ensure t
  :init
  (setq
   js2-mode-show-strict-warnings nil
   js2-mode-show-parse-errors nil
   js-indent-level 2
   js2-basic-offset 2
   sgml-basic-offset 2
   js2-strict-trailing-comma-warning nil
   js2-strict-missing-semi-warning nil)

  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
  (add-hook 'rjsx-mode-hook #'set-flow-executable)
  (add-hook 'rjsx-mode-hook #'setup-tide-mode)
  (add-hook 'rjsx-mode-hook #'init-company-flow)
  (add-hook 'rjsx-mode-hook #'init-flycheck)
  (add-hook 'rjsx-mode-hook #'prettier-js-mode)
  (add-hook 'rjsx-mode-hook #'init-js-indent-line))


(provide 'lang-javascript)
;;; javascript.el ends here
