;;; FOUNDATION ---------------------------------------------------------------------------

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

;;; MAC ----------------------------------------------------------------------------------

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
  (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend))

;;; ELISP --------------------------------------------------------------------------------

(defmacro diminish-major-mode (mode new-name)
  `(add-hook (intern (concat (symbol-name ,mode) "-hook"))
             '(lambda () (setq mode-name ,new-name))))

(diminish-major-mode 'lisp-mode "λ")
(diminish-major-mode 'emacs-lisp-mode "ξλ")

(use-package eldoc :diminish "")

;; modern library functions
(use-package dash :ensure t)
(use-package dash-functional :ensure t)

(use-package free-keys :ensure t)

;;; BUFFERS ------------------------------------------------------------------------------

;; auto refresh buffers
(setq global-auto-revert-mode t
      global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; directory prefix for better buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(use-package ibuffer :ensure t
  :config (diminish-major-mode 'ibuffer "≣")
  :bind (("C-x C-b" . ibuffer)))

(use-package ibuffer-vc :ensure t
  :after ibuffer
  :hook (ibuffer . ibuffer-vc-set-filter-groups-by-vc-root))

;; Easy workspaces creation and switching
(use-package eyebrowse :ensure t
  :config
  (setq eyebrowse-new-workspace t)
  (eyebrowse-mode t))

;; example title: EmacsConfig [1/4] | configuration.org

(defun my-title-bar-format()
  (let* ((current-slot (eyebrowse--get 'current-slot))
         (window-configs (eyebrowse--get 'window-configs))
         (window-config (assoc current-slot window-configs))
         (window-config-name (nth 2 window-config))
         (num-slots (length window-configs)))
    (concat window-config-name " [" (number-to-string current-slot)
            "/" (number-to-string num-slots) "] " "%b")))

(when (display-graphic-p)
  (setq frame-title-format '(:eval (my-title-bar-format))))

;;; DISPLAY ------------------------------------------------------------------------------

(setq display-fill-column-indicator-column 90)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;;(set-frame-font "Iosevka Nerd Font Mono 18")
(set-frame-font "Monofur Nerd Font Mono 19")

;; highlight line at point
(global-hl-line-mode 1)

;; see hex colours
(use-package rainbow-mode :ensure t
  :hook (prog-mode text-mode))

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
  :config
  (setq color-theme-is-global t
        solarized-high-contrast-mode-line t
        solarized-use-less-bold t
        solarized-emphasize-indicators nil
        solarized-scale-org-headlines nil
        x-underline-at-descent-line t)
  (load-theme 'solarized-light 'no-confirm))

;;; KEYS ---------------------------------------------------------------------------------
;; `bind-key` is only available after use-package

;; delete all whitespace at once (works with smart parens)
(use-package hungry-delete :ensure t
  :config (global-hungry-delete-mode))

;; make M-< and M-> sensible for common modes
(use-package beginend :ensure t
  :config (beginend-setup-all))

(global-unset-key (kbd "C-x C-+"))
(bind-key "C-s-+" 'text-scale-increase)
(bind-key "C-s--" 'text-scale-decrease)

(global-unset-key (kbd "C-x C-f"))
(bind-key "C-x f" 'find-file)

(global-unset-key (kbd "C-r")) ;; reverse-search
(global-unset-key (kbd "C-z")) ;; suspend-frame

;;; SEARCH/SUGGESTION --------------------------------------------------------------------

;; compound key suggestion
(use-package which-key :ensure t
  :diminish which-key-mode
  :config (which-key-mode +1))

(use-package smex :ensure t
  :init (setq smex-save-file (concat user-emacs-directory ".smex-items")))

(defun d/ignore-dired-buffers (str)
  "Return non-nil if STR names a Dired buffer.
This function is intended for use with `ivy-ignore-buffers'."
  (let ((buf (get-buffer str)))
    (and buf (eq (buffer-local-value 'major-mode buf) 'dired-mode))))

;; ivy, counsel and swiper for completion
(use-package counsel :ensure t
  :init (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t
        ivy-use-selectable-prompt t
        enable-recursive-minibuffers t
        ivy-count-format "(%d/%d) " ; space before end of string
        ivy-initial-inputs-alist nil ; don't insert leading '^'
        ivy-extra-directories nil)
  (add-to-list 'ivy-ignore-buffers #'d/ignore-dired-buffers)
  :bind (("C-s" . swiper-isearch)
         ("C-r" . ivy-resume)
         ("M-y" . counsel-yank-pop)
         :map ivy-minibuffer-map
         ("<return>" . ivy-alt-done)
         ;; ("C-f" . )
         ;; ("C-b" . )
         ))

;; spelling
(use-package flyspell
  :config (setq ispell-program-name "aspell"
                ispell-extra-args '("--sug-mode=ultra"
                                    "--camel-case"
                                    "--lang=en_CA"))
  :hook ((prog-mode text-mode) . flyspell-mode)
  :config (bind-key "C-." nil flyspell-mode-map))

(use-package flyspell-correct :ensure t :after flyspell
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

;; (this mode has versions for avy, helm, popup, etc...)
(use-package flyspell-correct-ivy :ensure t :after flyspell-correct)

;;; FILES --------------------------------------------------------------------------------

;; view archives (ensure read-only to avoid editing jar files)
(use-package arc-mode :ensure t
  :init (add-hook 'archive-extract-hook (lambda () (toggle-read-only 1))))

;; git
(use-package magit :ensure t
  :bind ("C-x m" . magit-status)
  :config (setq magit-display-buffer-function
                #'magit-display-buffer-fullframe-status-v1))

;;; EDITING ------------------------------------------------------------------------------

(setq-default indent-tabs-mode nil) ; Always use spaces for indent
(setq tab-width 2
      standard-indent 2
      shift-select-mode nil
      mouse-yank-at-point t)
(delete-selection-mode)

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
;;(use-package company-emoji :ensure t)
;;(use-package company-quickhelp :ensure t
;;  :config (company-quickhelp-mode 1))
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
(use-package smartparens :ensure t
  :diminish "()"
  :hook ((prog-mode . smartparens-mode)
         (prog-mode . smartparens-strict-mode)
         (cider-repl-mode . smartparens-mode)
         (cider-repl-mode . smartparens-strict-mode))
  :config
  (require 'smartparens-config) ;; default config for different languages.
  (show-smartparens-global-mode t) ;; highlights the paren that matches paren at point
  (bind-key "C-<left>" nil smartparens-mode-map)
  (bind-key "C-<right>" nil smartparens-mode-map)
  (bind-key "C-M-u" nil smartparens-mode-map)
  (bind-key "C-M-S-u" nil smartparens-mode-map)
  :bind (;; --- navigation ---
         ("C-M-a" . sp-beginning-of-sexp)
         ("C-M-e" . sp-end-of-sexp)
         ("C-M-S-a" . beginning-of-defun)
         ("C-M-S-e" . end-of-defun)
         ("C-M-f" . sp-forward-sexp)
         ("C-M-b" . sp-backward-sexp)
         ; unshifted operates on the left, shifted operates on the right
         ("C-M-n" . sp-down-sexp)
         ("C-M-S-n" . sp-up-sexp)
         ("C-M-p" . sp-backward-up-sexp)
         ("C-M-S-p" . sp-backward-down-sexp)
         ;; --- manipulation --
         ("C-M-r" . sp-rewrap-sexp)
         ("C-M-d" . sp-splice-sexp)
         ("C-M-," . sp-backward-slurp-sexp)
         ("C-M-." . sp-forward-slurp-sexp)
         ("C-M-<" . sp-backward-barf-sexp)
         ("C-M->" . sp-forward-barf-sexp)
         ("C-M-;" . sp-add-to-previous-sexp)
         ("C-M-'" . sp-add-to-next-sexp)
         ("C-M-/" . sp-split-sexp)
         ("C-M-?" . sp-join-sexp)
         ;; --- selection -----
         ("C-M-}" . sp-select-next-thing)
         ("C-M-{" . sp-select-previous-thing-exchange)))

(use-package expand-region :ensure t
  :bind ("C-=" . er/expand-region))

;;; PROGRAMMING --------------------------------------------------------------------------

(use-package coffee-mode :ensure t
  :config (diminish-major-mode 'coffee-mode "cs")
  :custom (coffee-tab-width 2))

(use-package fish-mode :ensure t
  :custom (fish-indent-offset 2))

(use-package dockerfile-mode :ensure t)

;; clojure -------------------------------------------------

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
    (-> 1)
    (->> 1)
    (some-> 1)
    (some->> 1)
    (cond-> 1)
    (cond->> 1)
    (as-> 2)
    ;; compojure
    (context 2)
    (POST 2))
  :hook ((clojure-mode . cider-mode)
         (cider-mode . eldoc-mode)
         (cider-connected . cider-enable-on-existing-clojure-buffers))
  :config
  (diminish-major-mode 'cider-repl-mode "Ⅽλ»")
  (add-to-list 'same-window-buffer-names "*cider*"))

(use-package eval-sexp-fu :ensure t
  :config (custom-set-faces '(eval-sexp-fu-flash
                              ((t (:foreground "green4" :weight bold))))))
(use-package cider-eval-sexp-fu :ensure t)

(use-package clj-refactor :ensure t
  :diminish ""
  :hook (clojure-mode . (lambda ()
                          (clj-refactor-mode 1)
                          (cljr-add-keybindings-with-prefix "C-c RET"))))

;; elixir --------------------------------------------------

(use-package elixir-mode :ensure t
  :config (diminish-major-mode 'elixir-mode "exλ"))

;;; DATA ---------------------------------------------------------------------------------


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

;;; TEXT ---------------------------------------------------------------------------------

(use-package markdown-mode :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

;;; SERVER (LAST) ------------------------------------------------------------------------

(require 'server)
(if (not (server-running-p)) (server-start))
