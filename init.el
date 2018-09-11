;;; Package configs

(defvar default-shell "/bin/zsh")
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

;; Ensuring user shell vars
(let ((path (shell-command-to-string (concat ". " default-shell-env-file "; echo -n $PATH"))))
  (setenv "PATH" path)
  (setq exec-path
        (append
         (split-string-and-unquote path ":")
         exec-path)))


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

;; [Languages]
(add-to-list 'load-path (concat user-emacs-directory "lang"))

(require 'lang-javascript)
(require 'lang-yaml)
(require 'lang-python)
(require 'lang-markdown)

;; Auto generated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "b54826e5d9978d59f9e0a169bbd4739dd927eead3ef65f56786621b53c031a7c" "0bff60fb779498e69ea705825a2ca1a5497a4fccef93bf3275705c2d27528f2f" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "b35a14c7d94c1f411890d45edfb9dc1bd61c5becd5c326790b51df6ebf60f402" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "356e5cbe0874b444263f3e1f9fffd4ae4c82c1b07fe085ba26e2a6d332db34dd" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "3a3de615f80a0e8706208f0a71bbcc7cc3816988f971b6d237223b6731f91605" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "b4c13d25b1f9f66eb769e05889ee000f89d64b089f96851b6da643cee4fdab08" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "3f44e2d33b9deb2da947523e2169031d3707eec0426e78c7b8a646ef773a2077" "8ec2e01474ad56ee33bc0534bdbe7842eea74dccfb576e09f99ef89a705f5501" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(flycheck-javascript-flow-args nil)
 '(package-selected-packages
   (quote
    (neotree color-theme-sanityinc-tomorrow exotica-theme helm-dash handlebars-mode zenburn-theme color-theme-solarized color-theme seti-theme company-jedi flymake-json ace-window csv-mode markdown-mode multi-term rainbow-mode beacon blacken dashboard restclient git-timemachine doom-modeline exec-path-from-shell yaml-mode company-tide flycheck-flow company-flow rjsx-mode indent-guide git-gutter helm-ag helm-projectile toggle-quotes doom-themes use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
