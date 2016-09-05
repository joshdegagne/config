(setq ns-use-srgb-colorspace nil)

;; Remove the UI
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))
(setq inhibit-startup-message t)
(set-fringe-mode '(1 . 1))
(setq use-dialog-box nil)

;; Make sure path is correct when launched as application
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(push "/usr/local/bin" exec-path)

;; Setup the package management
(require 'package)
(setq package-enable-at-startup nil)
(setq package-user-dir "~/.emacs.d/elpa/")
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa"        . "http://melpa.org/packages/") t)

(package-initialize)

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Load the configuration
(let ((user-config-file (expand-file-name (concat user-login-name ".el") user-emacs-directory)))
  (dolist (dir (list "lisp"
                     "config/pre"
                     "config/packages"
                     "config/languages"
                     "config/post"
                     user-login-name))
    (let ((config-dir (expand-file-name dir user-emacs-directory)))
      (when (file-exists-p config-dir)
        (add-to-list 'load-path config-dir)
        (mapc 'load (directory-files config-dir nil "^[^#].*el$")))))
  (when (file-exists-p user-config-file) (load user-config-file)))

;; Run the emacs server
(use-package server
  :if window-system
  :init (add-hook 'after-init-hook 'server-start t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-notify-p t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-dev))
 '(haskell-stylish-on-save t)
 '(haskell-tags-on-save t)
 '(nil nil t)
 '(package-selected-packages
   (quote
    (rust-snippets flycheck yaml-mode wgrep-ag visual-regexp-steroids use-package undo-tree transpose-frame string-utils solarized-theme smex smartscan slamhound shm shell-pop rainbow-mode rainbow-delimiters racer projectile powerline popwin markdown-mode+ magit latest-clojure-libraries kibit-helper js2-mode ido-ubiquitous ibuffer-vc highlight-symbol haskell-mode goto-chg flx-ido fill-column-indicator expand-region es-mode elisp-slime-nav dired+ datomic-snippets csv-mode company-quickhelp clojure-mode-extra-font-locking cljsbuild-mode clj-refactor circe cider-eval-sexp-fu buffer-move browse-kill-ring beginend avy auto-indent-mode ag)))
 '(shell-pop-shell-type
   (quote
    ("eshell" "*eshell*"
     (lambda nil
       (eshell shell-pop-term-shell)))))
 '(shell-pop-universal-key "M-`")
 '(shell-pop-window-position "top")
 '(shell-pop-window-size 100))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eval-sexp-fu-flash ((t (:foreground "green4" :weight bold))))
 '(org-level-1 ((t (:inherit default :foreground "#cb4b16"))))
 '(org-level-2 ((t (:inherit default :foreground "#859900"))))
 '(org-level-3 ((t (:inherit default :foreground "#268bd2"))))
 '(org-level-4 ((t (:inherit default :foreground "#b58900"))))
 '(org-level-5 ((t (:inherit default :foreground "#2aa198"))))
 '(org-level-6 ((t (:inherit default :foreground "#859900"))))
 '(org-level-7 ((t (:inherit default :foreground "#dc322f"))))
 '(org-level-8 ((t (:inherit default :foreground "#268bd2")))))
