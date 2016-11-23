(use-package markdown-mode :ensure t
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)))

(use-package yaml-mode :ensure t)

(use-package make-mode :ensure t)

(use-package csv-mode :ensure t)
(add-to-list
 'auto-mode-alist
 '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload
  'csv-mode
  "csv-mode"
  "Major mode for editing comma-separated value files." t)

(use-package es-mode :ensure t
  :mode ("\\.es$" . es-mode))
