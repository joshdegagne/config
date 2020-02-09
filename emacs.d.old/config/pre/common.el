(require 'uniquify)
(setq make-backup-files nil
      auto-save-default nil
      visible-bell t
      uniquify-buffer-name-style 'forward)

(global-hl-line-mode 1)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; Tune the GC to kick in every 20MB instead of 0.76MB
(setq gc-cons-threshold 20000000)

;; Make it hard to quit Emacs - C-x Really Quit
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)

;; Text size
(global-unset-key (kbd "C-x C-+")) ; don't zoom like this
(bind-key "C-s-+" 'text-scale-increase)
(bind-key "C-s--" 'text-scale-decrease)

(use-package beginend :ensure t
  :config (beginend-setup-all))

(defmacro diminish-major-mode (mode new-name)
  `(add-hook (intern (concat (symbol-name ,mode) "-hook"))
             '(lambda () (setq mode-name ,new-name))))

(use-package dash :ensure t)
