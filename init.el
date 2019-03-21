;;; Package configs

(defvar default-shell "/bin/bash")
(defvar default-shell-env-file "~/.zshenv")

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; [General changes]

;; Defining ZSH as default shell
(setenv "SHELL" default-shell)
(setq-default explicit-shell-file-name default-shell)

;; Using keychain env
"Set the environment variables `SSH_AUTH_SOCK' and `SSH_AGENT_PID'
 in Emacs' `process-environment' according to information retrieved 
 from files created by the keychain script."
  
(let* ((ssh (shell-command-to-string "keychain -q --noask --agents ssh --eval")))
  (list (and ssh
             (string-match "SSH_AUTH_SOCK[=\s]\\([^\s;\n]*\\)" ssh)
             (setenv       "SSH_AUTH_SOCK" (match-string 1 ssh)))
        (and ssh
             (string-match "SSH_AGENT_PID[=\s]\\([0-9]*\\)?" ssh)
             (setenv       "SSH_AGENT_PID" (match-string 1 ssh)))))


;; Ensuring user shell PATH
(let ((path (shell-command-to-string (concat ". " default-shell-env-file "; echo -n $PATH"))))
  (setenv "PATH" path)
  (setq exec-path
        (append
         (split-string-and-unquote path ":")
         exec-path)))

;; Ensuring cal settings
(let ((cal_id (shell-command-to-string (concat ". " default-shell-env-file "; echo -n $CAL_ID"))))
  (setenv "CAL_ID" cal_id))
(let ((cal_secret (shell-command-to-string (concat ". " default-shell-env-file "; echo -n $CAL_SECRET"))))
  (setenv "CAL_SECRET" cal_secret))


;; Avoid lock files
(setq create-lockfiles nil)

;; Disable backup files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq create-lockfiles nil)

;; [Modules]
(add-to-list 'load-path (concat user-emacs-directory "modules"))

(require 'utils)
(require 'core)
(require 'ui)
(require 'keybinding)
(require 'org-settings)

;; [Languages]
(add-to-list 'load-path (concat user-emacs-directory "lang"))

(require 'lang-javascript)
(require 'lang-yaml)
(require 'lang-python)
(require 'lang-markdown)
(require 'lang-web)

;; Auto generated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-javascript-flow-args nil)
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (sphinx-doc python-docstring docker nginx-mode org-agenda org-gcal org-g org-cal list-environment web-mode dockerfile-mode helm-dash handlebars-mode zenburn-theme color-theme-solarized color-theme seti-theme company-jedi flymake-json ace-window csv-mode markdown-mode multi-term rainbow-mode beacon blacken dashboard restclient git-timemachine doom-modeline exec-path-from-shell yaml-mode company-tide flycheck-flow company-flow rjsx-mode indent-guide git-gutter helm-ag helm-projectile toggle-quotes doom-themes use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
