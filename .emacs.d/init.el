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
