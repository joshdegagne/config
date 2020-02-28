(use-package clojure-mode :ensure t
  :init (progn
          (setq buffer-save-without-query t)
          (add-hook 'clojure-mode-hook (lambda () (lisp-mode-setup)))
          (add-hook 'clojure-mode-hook 'highlight-symbol-mode))
  :config (progn
            (diminish-major-mode 'clojure-mode "Cλ")
            (bind-key "C-c C-z" nil clojure-mode-map))) ; Remove the binding for inferior-lisp-mode

(use-package clojure-mode-extra-font-locking :ensure t)

(use-package cider :ensure t
  :init (progn
          (setq nrepl-hide-special-buffers nil
                cider-repl-pop-to-buffer-on-connect nil
                cider-prompt-for-symbol nil
                nrepl-log-messages t
                cider-popup-stacktraces t
                cider-repl-popup-stacktraces t
                cider-auto-select-error-buffer t
                cider-repl-print-length 100
                cider-repl-history-file (expand-file-name "cider-history" user-emacs-directory)
                cider-repl-use-clojure-font-lock t
                cider-switch-to-repl-command 'cider-switch-to-relevant-repl-buffer)
          (define-clojure-indent
            (match 1)
            ;; compojure
            (context 2)
            (POST 2))
          (add-hook 'clojure-mode-hook 'cider-mode))
  :config (progn
            (diminish-major-mode 'cider-repl-mode "Ç»")
            (add-to-list 'same-window-buffer-names "*cider*")
            (add-hook 'cider-mode-hook 'eldoc-mode)
            (add-hook 'cider-repl-mode-hook 'lisp-mode-setup)
            (add-hook 'cider-connected-hook 'cider-enable-on-existing-clojure-buffers))
  :diminish " ç")

(use-package eval-sexp-fu :ensure t
  :init (custom-set-faces '(eval-sexp-fu-flash ((t (:foreground "green4" :weight bold))))))

(use-package cider-eval-sexp-fu :ensure t)

(use-package clj-refactor :ensure t
  :init (add-hook 'clojure-mode-hook (lambda ()
                                       (clj-refactor-mode 1)
                                       (cljr-add-keybindings-with-prefix "C-c RET")))
  :diminish "")

(use-package cljsbuild-mode :ensure t)

(use-package datomic-snippets :ensure t)

(use-package kibit-helper :ensure t)