;;; Web Mode

(use-package web-mode
  :ensure t
  :mode (("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.jinja2?\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.tpl\\'" . web-mode)))

(provide 'lang-web)
;;; lang-web ends here
