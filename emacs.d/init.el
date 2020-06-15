;;; FOUNDATION -----------------------------------------------------------------

(setq user-full-name "Joshua DeGagné"
      user-mail-address "joshdegagne@gmail.com")

;; assume the use of symlinks to swap configurations
(setq user-emacs-directory (file-truename "~/.emacs.d/"))

(setq gc-cons-threshold 50000000) ; 50mb
(setq large-file-warning-threshold 100000000) ; 100mb
(push "/usr/local/bin" exec-path)

;; remove the ui
(when (window-system)
  (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode horizontal-scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1))))
(set-fringe-mode '(1 . 1))
(setq inhibit-startup-message t
      inhibit-startup-screen t
      use-dialog-box nil
      visible-bell t
      line-number-mode t
      column-number-mode t
      size-indication-mode t)

;; scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; don't pollute init.el with generated elisp
(setq custom-file (concat user-emacs-directory "generated.custom.el"))
(when (file-exists-p custom-file) (load custom-file))

;; file management
(setq make-backup-files nil
      backup-directory-alist '(("" . (concat user-emacs-directory "backups")))
      auto-save-default nil)

;; auto refresh buffers
(setq global-auto-revert-mode t
      global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(desktop-save-mode 1)

;; answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; whitespace
(setq show-trailing-whitespace t)
(add-hook 'before-save-hook 'whitespace-cleanup)

;; setup use-package
(require 'package)
(setq package-enable-at-startup nil
      package-user-dir (concat user-emacs-directory "elpa/"))
(add-to-list 'package-archives    '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives    '("melpa"        . "http://melpa.org/packages/") t)
(setq package-archive-priorities '(("melpa-stable" . 20)
                                   ("melpa"        . 10)))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

;; allow use-package for git-repos
(use-package quelpa-use-package :ensure t)

(use-package diminish :ensure t)

;;; MAC ------------------------------------------------------------------------

(when (equal system-type 'darwin)
  ;; move to trash when deleting stuff
  (setq delete-by-moving-to-trash t
        trash-directory "~/.Trash/emacs")

  ;; Always open a file in a new frame
  (setq ns-pop-up-frames t)

  ;; use finder
  (use-package reveal-in-osx-finder :ensure t)

  ;; The osx ls does not support -X or --sort
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil)

  ;; powerline colour rendering bug when running on osx.
  ;; (setq powerline-image-apple-rgb t)
  (setq ns-use-srgb-colorspace nil)

  ;; Useful for https://github.com/dunn/company-emoji
  ;; https://www.reddit.com/r/emacs/comments/8ph0hq/i_have_converted_from_the_mac_port_to_the_ns_port/
  ;; not tested with emacs26 (requires a patched Emacs version for multi-color font support)
  (if (version< "27.0" emacs-version)
      (set-fontset-font "fontset-default"
                        'unicode "Apple Color Emoji" nil 'prepend)
    (set-fontset-font t
                      'symbol
                      (font-spec :family "Apple Color Emoji") nil 'prepend)))

;;; ELISP ----------------------------------------------------------------------

(defmacro diminish-major-mode (mode new-name)
  `(add-hook (intern (concat (symbol-name ,mode) "-hook"))
             '(lambda () (setq mode-name ,new-name))))

(diminish-major-mode 'lisp-mode "λ")
(diminish-major-mode 'emacs-lisp-mode "ξλ")

(use-package eldoc :diminish "")

;; modern library functions
(use-package dash :ensure t)
(use-package dash-functional :ensure t)

;;; DISPLAY --------------------------------------------------------------------

;; directory prefix for better buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; highlight line at point
(global-hl-line-mode 1)

;; visually identify matching parens
(use-package rainbow-delimiters :ensure t
  :diminish rainbow-delimiters
  :hook ((prog-mode text-mode) . rainbow-delimiters-mode))

(use-package powerline :ensure t
  :config
  ;; Included separators: alternate, arrow, arrow-fade, bar, box, brace, butt,
  ;; chamfer, contour, curve, rounded, roundstub, slant, wave, zigzag, and nil.
  (setq powerline-default-separator 'slant)
  ;; Included themes: default, center, center-evil, vim, and nano.
  (powerline-default-theme))

;; theme
(use-package solarized-theme :ensure t
  :init
  (setq solarized-high-contrast-mode-line t
        solarized-use-less-bold t
        solarized-emphasize-indicators nil
        solarized-scale-org-headlines nil
        x-underline-at-descent-line t)
  (load-theme 'solarized-light 'no-confirm)
  :config (setq color-theme-is-global t))

;; font
(when (window-system)
  (set-frame-font "Fira Code 16")
  (use-package fira-code-mode :ensure t
    :diminish ""
    :hook (prog-mode text-mode)))



;;; KEYS -----------------------------------------------------------------------
;;(`bind-key` is only available after use-package)

;; zoom
(global-unset-key (kbd "C-x C-+"))
(bind-key "C-s-+" 'text-scale-increase)
(bind-key "C-s--" 'text-scale-decrease)

;; make M-< and M-> sensible for common modes
(use-package beginend :ensure t
  :config (beginend-setup-all))

(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; SEARCH/SUGGESTION ----------------------------------------------------------

;; compound key suggestion
(use-package which-key :ensure t
  :diminish which-key-mode
  :config (which-key-mode +1))

(use-package smex :ensure t
  :init (setq smex-save-file (concat user-emacs-directory ".smex-items")))

;; ivy, counsel and swiper for completion
(use-package counsel :ensure t
  ;;need 27 :diminish ('ivy-mode "🔰")
  :init (ivy-mode 1)
  :config (setq ivy-use-virtual-buffers t
                enable-recursive-minibuffers t
                ivy-count-format "(%d/%d) " ; space before end of string
                ivy-initial-inputs-alist nil ; don't insert leading '^'
                )
  :bind (("C-s" . swiper-isearch)
         ("M-y" . counsel-yank-pop)))

;; spelling
(use-package flyspell
  :config (setq ispell-program-name "aspell"
                ispell-extra-args '("--sug-mode=ultra"
                                    "--camel-case"
                                    "--lang=en_CA"))
  :hook ((prog-mode text-mode) . flyspell-mode))

(use-package flyspell-correct :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

;; (this mode has versions for avy, helm, popup, etc...)
(use-package flyspell-correct-ivy :ensure t :after flyspell-correct)

;;; FILES ----------------------------------------------------------------------

(use-package dired+ :ensure t
  :quelpa (dired+ :fetcher github :repo "emacsmirror/dired-plus")
  :init (diredp-toggle-find-file-reuse-dir 1))

;; view archives (ensure read-only to avoid editing jar files)
(use-package arc-mode :ensure t
  :init (add-hook 'archive-extract-hook (lambda () (toggle-read-only 1))))

;; git
(use-package magit :ensure t
  :bind ("C-x m" . magit-status)
  :config (setq magit-display-buffer-function
                #'magit-display-buffer-fullframe-status-v1))

;;; EDITING --------------------------------------------------------------------

(setq-default indent-tabs-mode nil) ; Always use spaces for indent
(setq tab-width 2
      standard-indent 2
      shift-select-mode nil
      mouse-yank-at-point t)
(delete-selection-mode)

;; support the "dangerous" commands :-)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(use-package undo-tree :ensure t
  :diminish ""
  :config
  (diminish-major-mode 'undo-tree-visualizer-mode "⅄")
  (global-undo-tree-mode)
  (setq undo-tree-show-minibuffer-help t))

;; "tree" jumping
(use-package avy :ensure t
  :bind (("C-." . avy-goto-word-or-subword-1)))

;; jump to symbol @ point
(use-package smartscan :ensure t
  :hook (prog-mode . smartscan-mode)
  :config (setq smartscan-symbol-selector "symbol"))

(use-package highlight-symbol :ensure t
  :hook (prog-mode . highlight-symbol-mode))

(use-package multiple-cursors :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)))

;; C-a to beginning of indented stuff
(use-package crux :ensure t
  :bind (("C-a" . crux-move-beginning-of-line)))

;; navigation within camelCase words
(use-package subword :ensure t
  :diminish ""
  :hook (prog-mode . subword-mode))

;; completion box
(use-package company-emoji :ensure t)
(use-package company-quickhelp :ensure t
  :config (company-quickhelp-mode 1))
(use-package company :ensure t
  :diminish " α"
  :config
  (setq company-idle-delay 0.2
              company-tooltip-limit 10
              company-minimum-prefix-length 2
              company-tooltip-flip-when-above t)
  (global-company-mode 1)
  (add-to-list 'company-backends 'company-emoji))

;; keep things balanced automatically
;; needs help with elisp quoting
(use-package  smartparens :ensure t
  :diminish "()"
  :hook ((prog-mode . smartparens-mode)
         (prog-mode . smartparens-strict-mode))
  :config
  (require 'smartparens-config)
  ;; highlights the paren that matches paren at point
  (show-smartparens-global-mode t)
  ;; :bind ; need to add shortcuts for stuff (slurp, barf, wrap, unwrap, etc...)
  )

;; semantic selections
(use-package expand-region :ensure t
  :bind ("C-=" . er/expand-region))

;;; PROGRAMMING ----------------------------------------------------------------

(use-package coffee-mode :ensure t
  :custom (coffee-tab-width 2)
  :config (diminish-major-mode 'coffee-mode "☕𝒿𝓈"))

(use-package fish-mode :ensure t
  ;; need 27 :config (diminish-major-mode 'fish-mode "🐟")
  :custom (fish-indent-offset 2))

(use-package dockerfile-mode :ensure t)

(use-package clojure-mode :ensure t
  :init (setq buffer-save-without-query t)
  :bind ("C-c C-z" . clojure-mode-map) ; Remove the binding for inferior-lisp-mode
  :config (diminish-major-mode 'clojure-mode "Ⅽλ"))
(use-package clojure-mode-extra-font-locking :ensure t)
(use-package cider :ensure t
  :diminish " ⅽ"
  :init
  (setq nrepl-hide-special-buffers nil
        cider-repl-pop-to-buffer-on-connect nil
        cider-prompt-for-symbol nil
        nrepl-log-messages t
        cider-popup-stacktraces t
        cider-repl-popup-stacktraces t
        cider-auto-select-error-buffer t
        cider-repl-print-length 100
        cider-repl-history-file (concat user-emacs-directory "cider-history")
        cider-repl-use-clojure-font-lock t
        cider-switch-to-repl-command 'cider-switch-to-relevant-repl-buffer)
  (define-clojure-indent
    (match 1)
    ;; compojure
    (context 2)
    (POST 2))
  :hook ((clojure-mode . cider-mode)
         (cider-mode . eldoc-mode)
         (cider-connected . cider-enable-on-existing-clojure-buffers))
  :config (progn
            (diminish-major-mode 'cider-repl-mode "Ⅽλ»")
            (add-to-list 'same-window-buffer-names "*cider*")))

(use-package eval-sexp-fu :ensure t
  :config (custom-set-faces '(eval-sexp-fu-flash
                              ((t (:foreground "green4" :weight bold))))))
(use-package cider-eval-sexp-fu :ensure t)

(use-package clj-refactor :ensure t
  :diminish ""
  :hook (clojure-mode . (lambda ()
                          (clj-refactor-mode 1)
                          (cljr-add-keybindings-with-prefix "C-c RET"))))

;;; DATA -----------------------------------------------------------------------

(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

(use-package yaml-mode :ensure t
  :mode "\\.ya?ml\\'")

(use-package csv-mode :ensure t
  :mode "\\.[Cc][Ss][Vv]\\'")

(use-package nix-mode
  :mode "\\.nix\\'")

;;; TEXT -----------------------------------------------------------------------

(use-package markdown-mode :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

;;; SERVER (LAST) --------------------------------------------------------------

(require 'server)
(if (not (server-running-p)) (server-start))
