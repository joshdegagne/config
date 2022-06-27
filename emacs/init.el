;;; FOUNDATION ---------------------------------------------------------------------------

(setq user-full-name "Joshua DeGagné"
      user-mail-address "joshdegagne@gmail.com")

;; assume the use of symlinks to swap configurations
(setq user-emacs-directory (file-truename "~/.emacs.d/"))

(setq gc-cons-threshold (* 50 1024 1024)) ; 50mb
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
      column-number-mode t
      size-indication-mode t
      frame-resize-pixelwise t)
(global-display-line-numbers-mode 1)

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
      package-user-dir (concat user-emacs-directory "elpa/")
      package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/"))
      package-archive-priorities '(("melpa" . 20)
                                   ("melpa-stable" . 10)))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

(add-to-list 'imenu-generic-expression
             '("Used Packages"
               "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2))

(use-package exec-path-from-shell)

;; allow use-package for git-repos
(use-package quelpa-use-package)

(use-package delight)
(use-package delight-powerline
  :quelpa (:fetcher wiki :repo "https://www.emacswiki.org/emacs/delight-powerline.el")
  :after (delight powerline))

;;; MAC ----------------------------------------------------------------------------------

(when (equal system-type 'darwin)

  ;; move to trash when deleting stuff
  (setq delete-by-moving-to-trash t
        trash-directory "~/.Trash/emacs")

  ;; properly set up path
  (exec-path-from-shell-initialize)

  ;; Always open a file in a new frame
  (setq ns-pop-up-frames t)

  ;; use finder
  (use-package reveal-in-osx-finder)

  ;; The osx ls does not support -X or --sort
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

;;; ELISP --------------------------------------------------------------------------------

(use-package dash)

(defmacro ivy-quit-and-run (&rest body)
  "Quit the minibuffer and run BODY afterwards."
  `(progn
     (put 'quit 'error-message "")
     (run-at-time nil nil
                  (lambda ()
                    (put 'quit 'error-message "Quit")
                    ,@body))
     (minibuffer-keyboard-quit)))

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defun replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

;;; BUFFERS ------------------------------------------------------------------------------

;; control how "popup" buffers open so thy don't just split some random window
(setq display-buffer-base-action '((display-buffer-in-side-window) . ((side . bottom))))

;; auto refresh buffers
(global-auto-revert-mode t)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; directory prefix for better buffer names
(use-package uniquify
  :ensure nil ; base emacs
  :config (setq uniquify-buffer-name-style 'forward))

;; TODO: might replace ibuffer with bufler

(use-package ibuffer
  :config (delight '((ibuffer "" :major)))
  :bind (("C-x C-b" . ibuffer)))

(use-package ibuffer-vc
  :after ibuffer
  :hook (ibuffer . ibuffer-vc-set-filter-groups-by-vc-root))

;;; DISPLAY ------------------------------------------------------------------------------

(setq display-fill-column-indicator-column 100)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

(set-frame-font "Monofur Nerd Font Mono 19" nil t)

;; highlight line at point
(global-hl-line-mode 1)

;; see hex colours
(use-package rainbow-mode
  :delight
  :hook (prog-mode text-mode))

;; visually identify matching parens
(use-package rainbow-delimiters
  :delight
  :hook ((prog-mode text-mode) . rainbow-delimiters-mode))

(use-package powerline
  :config
  ;; Included separators: alternate, arrow, arrow-fade, bar, box, brace, butt,
  ;; chamfer, contour, curve, rounded, roundstub, slant, wave, zigzag, and nil.
  (setq powerline-default-separator 'wave)
  ;; Included themes: default, center, center-evil, vim, and nano.
  (powerline-default-theme))

;; theme
(use-package solarized-theme
  :config
  (setq color-theme-is-global t
        solarized-distinct-fringe-background t
        solarized-high-contrast-mode-line t
        solarized-use-less-bold t
        solarized-emphasize-indicators nil
        solarized-use-more-italic t
        solarized-scale-org-headlines nil
        x-underline-at-descent-line t)
  (load-theme 'solarized-light 'no-confirm))

(use-package tab-bar
  :ensure nil
  :after powerline
  :config
  (setq tab-bar-separator ""
        tab-bar-close-button-show nil
        tab-bar-new-button-show nil)
  (set-face-attribute 'tab-bar nil
                      :inherit 'powerline-inactive1
                      :background nil
                      :foreground nil)
  (set-face-attribute 'tab-bar-tab nil
                      :inherit 'powerline-active0
                      :background nil
                      :foreground nil)
  (set-face-attribute 'tab-bar-tab-inactive nil
                      :inherit 'powerline-inactive0
                      :background nil
                      :foreground nil)
  (defvar my/tab-bar-height 25)
  (defvar my/tab-bar-left (powerline-wave-right 'tab-bar 'tab-bar-tab my/tab-bar-height))
  (defvar my/tab-bar-right (powerline-wave-left 'tab-bar-tab 'tab-bar my/tab-bar-height))
  (defvar my/tab-bar-left-inactive (powerline-wave-right 'tab-bar 'tab-bar-tab-inactive my/tab-bar-height))
  (defvar my/tab-bar-right-inactive (powerline-wave-left 'tab-bar-tab-inactive 'tab-bar my/tab-bar-height))
  (defun my/tab-bar-tab-name-format (tab i)
    (let ((current-p (eq (car tab) 'current-tab)))
      (powerline-render
       (list (if current-p my/tab-bar-left my/tab-bar-left-inactive)
             (format "%s" (tab-bar-tab-name-format-default tab i))
             (if current-p my/tab-bar-right my/tab-bar-right-inactive)))))
  (tab-bar-mode 1)
  (setq tab-bar-tab-name-format-function #'my/tab-bar-tab-name-format))
;;; KEYS ---------------------------------------------------------------------------------
;; `bind-key` is only available after use-package

;; check what's available
(use-package free-keys)

;; delete all whitespace at once (works with smart parens)
(use-package hungry-delete
  :delight
  :config (global-hungry-delete-mode))
(add-hook 'minibuffer-setup-hook (lambda () (hungry-delete-mode -1)))

;; make M-< and M-> sensible for common modes
(use-package beginend
  :delight
  (beginend-global-mode)
  (beginend-dired-mode)
  (beginend-prog-mode)
  :config (beginend-setup-all))

;; zoooming
(global-unset-key (kbd "C-x C-+"))
;; s--, s-=, s-0

(global-unset-key (kbd "C-x C-f"))
(bind-key "C-x f" 'find-file)

(global-unset-key (kbd "C-r")) ;; reverse-search
(global-unset-key (kbd "C-z")) ;; suspend-frame

;;; SEARCH/SUGGESTION --------------------------------------------------------------------

(use-package projectile
  :delight '(:eval (format " [%s]" (projectile-project-name)))
  :init (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-x p" . projectile-command-map)))

(use-package projectile-ripgrep :after projectile)

(use-package counsel-projectile :after projectile
  :init (counsel-projectile-mode))

;; compound key suggestion
(use-package which-key
  :delight
  :config (which-key-mode +1))

(use-package smex
  :init (setq smex-save-file (concat user-emacs-directory ".smex-items")))

(defun d/ignore-dired-buffers (str)
  "Return non-nil if STR names a Dired buffer. This function is intended for use with `ivy-ignore-buffers'."
  (let ((buf (get-buffer str)))
    (and buf (eq (buffer-local-value 'major-mode buf) 'dired-mode))))

(use-package ivy :delight " ")

;; ivy, counsel and swiper for completion
(use-package counsel
  :init (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t
        ivy-use-selectable-prompt t
        enable-recursive-minibuffers t
        ivy-count-format "(%d/%d) " ; space before end of string
        ivy-initial-inputs-alist nil ; don't insert leading '^'
        ivy-extra-directories nil
        confirm-nonexistent-file-or-buffer t
        ivy-wrap t
        ivy-height 13)
  (add-to-list 'ivy-ignore-buffers #'d/ignore-dired-buffers)
  (ivy-configure 'counsel-imenu :update-fn 'auto)
  :bind (("M-i" . counsel-imenu)
         ("C-s" . swiper-isearch)
         ("C-r" . ivy-resume)
         ("M-y" . counsel-yank-pop)
         :map ivy-minibuffer-map
         ("<return>" . ivy-alt-done)
         ("C-f" . (lambda () (interactive) (ivy-quit-and-run (counsel-find-file))))
         ("C-b" . (lambda () (interactive) (ivy-quit-and-run (ivy-switch-buffer))))))

;; spelling
(use-package flyspell
  :delight " ﯑"
  :config (setq ispell-program-name "aspell"
                ispell-extra-args '("--sug-mode=ultra"
                                    "--camel-case"
                                    "--lang=en_CA")
                ispell-list-command "--list"
                flyspell-issue-message-flag nil)
  :hook ((prog-mode text-mode) . flyspell-mode)
  :config (bind-key "C-." nil flyspell-mode-map))
(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))
(use-package flyspell-correct-ivy
  :after flyspell-correct)

;;; FILES --------------------------------------------------------------------------------

;; view archives (ensure read-only to avoid editing jar files)
(use-package arc-mode
  :init (add-hook 'archive-extract-hook (lambda () (toggle-read-only 1))))

(use-package magit
  :bind ("C-x m" . magit-status)
  :config (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-topleft-v1
                magit-pre-display-buffer-hook #'magit-save-window-configuration
                magit-bury-buffer-function #'magit-restore-window-configuration
                ;; maybe not the best spot, but I almost exclusively use ediff for git diffs
                ediff-window-setup-function #'ediff-setup-windows-plain))


;;; EDITING ------------------------------------------------------------------------------

(setq-default indent-tabs-mode nil) ; Always use spaces for indent
(setq tab-width 2
      standard-indent 2
      shift-select-mode nil
      mouse-yank-at-point t)
(delete-selection-mode)

(use-package undo-tree
  :delight
  :config
  (delight 'undo-tree-visualizer-mode "" :major)
  (global-undo-tree-mode)
  (setq undo-tree-show-minibuffer-help t)
  ;; Prevent undo tree files from polluting your git repo
  (setq undo-tree-history-directory-alist
        `(("." . ,(concat user-emacs-directory "undo-tree")))))

(setq qwerty/home-row '(?a ?s ?d ?f ?j ?k ?l ?\;))
(setq workman/home-row '(?a ?s ?h ?t ?n ?e ?o ?i))
(setq home-row workman/home-row) ;; set to keyboard

;; "tree" jumping
(use-package avy
  :config
  (setq avy-timeout-seconds 0.3)
  (setq avy-keys home-row)
  :bind (("C-o" . avy-goto-char-timer)))

(use-package ace-window
  :config
  (setq aw-keys home-row)
  (setq aw-dispatch-alist ;; avoid home row keys (for both layouts)
        '((?x aw-delete-window "Delete Window")
          (?m aw-swap-window "Swap Windows")
          (?w aw-copy-window "Copy Window")
          (?p aw-flip-window "Go to Previous Window")
          (?f aw-split-window-fair "Split Fair Window")
          (?v aw-split-window-vert "Split Vert Window")
          (?c aw-split-window-horz "Split Horz Window")
          (?l delete-other-windows "Delete Other Windows")
          (?y aw-transpose-frame "Yeet Window to Other Frame")
          (?? aw-show-dispatch-help)))
  :bind (("M-o" . ace-window)))

;; jump to symbol @ point
(use-package smartscan
  :hook (prog-mode . smartscan-mode)
  :config (setq smartscan-symbol-selector "symbol"))

(use-package highlight-symbol
  :delight
  :hook (prog-mode . highlight-symbol-mode))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)))

;; C-a to beginning of indented stuff
(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)))

;; navigation within camelCase words
(use-package subword
  :delight
  :hook (prog-mode . subword-mode))

;; completion box
(use-package company
  :delight " "
  :config
  (setq company-idle-delay 0.2
        company-tooltip-limit 10
        company-minimum-prefix-length 2
        company-tooltip-flip-when-above t)
  (global-company-mode 1)
  (comment (add-to-list 'company-backends 'company-emoji)))
(use-package company-quickhelp
  :after company
  :config (company-quickhelp-mode 1))
(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))

;; keep things balanced automatically
(use-package smartparens
  :delight " "
  :hook ((prog-mode . smartparens-mode)
         (prog-mode . smartparens-strict-mode)
         (cider-repl-mode . smartparens-mode)
         (cider-repl-mode . smartparens-strict-mode))
  :config
  (require 'smartparens-config) ; default config for different languages.
  (show-smartparens-global-mode t) ; highlights the paren that matches paren at point
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
         ("C-M-p" . sp-backward-down-sexp)
         ("C-M-S-p" . sp-backward-up-sexp)
         ;; --- manipulation --
         ("C-M-<backspace>" . sp-backward-kill-sexp)
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

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package eldoc
  :delight)

;;; PROGRAMMING --------------------------------------------------------------------------

;; lisp ----------------------------------------------------

(delight '((lisp-mode "λ" :major)
           (emacs-lisp-mode "ξλ" :major)))

(use-package eval-sexp-fu
  :config (custom-set-faces '(eval-sexp-fu-flash
                              ((t (:foreground "green4" :weight bold))))))

;; clojure -------------------------------------------------

(use-package clojure-mode
  :init (setq buffer-save-without-query t)
  ;; Remove the binding for inferior-lisp-mode
  :bind ("C-c C-z" . clojure-mode-map)
  :config (delight '((clojure-mode " cljλ" :major)
                     (clojurescript-mode " cljsλ" :major)
                     (clojurec-mode "cljcλ" :major))))

(use-package clojure-mode-extra-font-locking)

(use-package cider
  :pin melpa-stable
  :delight '(:eval (format " [%s]" (replace-in-string " " "-" (cider--modeline-info))))
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
    ;;
    (assoc '(0 (:form)))
    ;; core.match pattern matching
    (match 1))
  :hook ((clojure-mode . cider-mode)
         (cider-mode . eldoc-mode)
         (cider-connected . cider-enable-on-existing-clojure-buffers))
  :config
  (delight 'cider-repl-mode "" :major)
  (add-to-list 'same-window-buffer-names "*cider*"))

(use-package cider-eval-sexp-fu
  :after eval-sexp-fu)

(comment (use-package clj-refactor
           :delight
           :hook ((clojure-mode . (lambda ()
                                    (clj-refactor-mode 1))))))

(use-package cljsbuild-mode)

;; elixir --------------------------------------------------

(use-package elixir-mode
  :config (delight 'elixir-mode "exλ" :major))

;;; DATA ---------------------------------------------------------------------------------

(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package csv-mode
  :mode "\\.[Cc][Ss][Vv]\\'")

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package dockerfile-mode)

;;; TEXT ---------------------------------------------------------------------------------

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc")
  :config (delight 'markdown-mode "md" :major))

;;; SERVER (LAST) ------------------------------------------------------------------------

(require 'server)
(if (not (server-running-p)) (server-start))
