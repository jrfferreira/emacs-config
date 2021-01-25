;;; Package configs

(defvar default-shell "/bin/zsh")
(defvar default-shell-env-file "~/.zshrc")

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
(require 'my-treemacs)

;; [Languages]
(add-to-list 'load-path (concat user-emacs-directory "lang"))

(require 'lang-javascript)
(require 'lang-yaml)
(require 'lang-python)
(require 'lang-markdown)
(require 'lang-web)
(require 'lang-rust)

;; Auto generated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("7f74a3b9a1f5e3d31358b48b8f8a1154aab2534fae82c9e918fb389fca776788" "2d1fe7c9007a5b76cea4395b0fc664d0c1cfd34bb4f1860300347cdad67fb2f9" "0d087b2853473609d9efd2e9fbeac088e89f36718c4a4c89c568dd1b628eae41" "a2286409934b11f2f3b7d89b1eaebb965fd63bc1e0be1c159c02e396afb893c8" default)))
 '(flycheck-javascript-flow-args nil)
 '(git-messenger:use-magit-popup t)
 '(org-agenda-files (quote ("~/Documents/work/org/work.org")))
 '(package-selected-packages
   (quote
    (yafolding treemacs-all-the-icons treemacs-magit treemacs-icons-dired treemacs-projectile treemacs elpy rainbow-delimiters json-navigator bnf-mode git-messenger editorconfig flycheck-rust rust-mode pyenv-mode sphinx-doc python-docstring docker nginx-mode org-agenda org-gcal org-g org-cal list-environment web-mode dockerfile-mode helm-dash handlebars-mode zenburn-theme color-theme-solarized color-theme seti-theme company-jedi flymake-json ace-window csv-mode markdown-mode multi-term rainbow-mode beacon blacken dashboard restclient git-timemachine doom-modeline exec-path-from-shell yaml-mode company-tide flycheck-flow company-flow rjsx-mode indent-guide git-gutter helm-ag helm-projectile toggle-quotes doom-themes use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-thing ((t (:inherit lazy-highlight)))))
(put 'downcase-region 'disabled nil)
