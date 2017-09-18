(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

(use-package coffee-mode :ensure t)
(custom-set-variables '(coffee-tab-width 2))
