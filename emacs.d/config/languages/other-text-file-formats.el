(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package yaml-mode :ensure t
  :mode ("\\.yaml\\'" . yaml-mode))

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

(use-package dockerfile-mode :ensure t)
