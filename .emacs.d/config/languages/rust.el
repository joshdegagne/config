(use-package rust-mode :ensure t
  :mode (("\\.rs$" . rust-mode))
  :config (progn
            (setq rust-indent-offset 4)
            (setq rust-indent-method-chain t)))

(use-package racer :ensure t
  :init (progn
          (setq racer-rust-src-path (getenv "RUST_SRC_PATH"))
          (setq racer-cmd "~/.cargo/bin/racer")
          (setq company-tooltip-align-annotations t)
          (setq company-minimum-prefix-length 2))
  :config (progn
            (add-hook 'rust-mode-hook #'racer-mode)
            (add-hook 'racer-mode-hook #'eldoc-mode)
            (add-hook 'racer-mode-hook #'company-mode)
            (add-hook 'rust-mode-hook
                      '(lambda ()
                         (local-set-key (kbd "M-.") #'racer-find-definition)))))

(add-to-list 'auto-mode-alist '("\\.toml$" . prog-mode))
