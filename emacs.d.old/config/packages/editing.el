;; Global defaults
(setq-default indent-tabs-mode nil) ; Always use spaces for indent
(setq tab-width 2
      standard-indent 2
      line-number-mode t
      column-number-mode t
      sentence-end-double-space t
      shift-select-mode nil
      mouse-yank-at-point t)
(delete-selection-mode)

;; support the "dangerous" commands :-)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(add-hook 'text-mode-hook
          (lambda ()
            (setq mode-name "Ƭ")
            (turn-on-auto-fill)))

(use-package expand-region :ensure t
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

(use-package undo-tree :ensure t
  :config (progn (global-undo-tree-mode)
                 (diminish-major-mode 'undo-tree-visualizer-mode "⅄"))
  :diminish ((undo-tree-mode . "")))

(use-package browse-kill-ring :ensure t
  :config (browse-kill-ring-default-keybindings))

(use-package avy :ensure t
  :bind (("C-." . avy-goto-word-or-subword-1)
         ("C-o" . avy-goto-word-or-subword-1)))

(use-package multiple-cursors :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-c C-e" . mc/edit-ends-of-lines)))

;; Ensure that when we navigate to a file in an archive, it is opened as
;; read-only by default. Primarily there to prevent unintentional editing
;; of jar files
(use-package arc-mode
  :init (add-hook 'archive-extract-hook (lambda () (toggle-read-only 1))))

(use-package goto-chg :ensure t
  :bind (("C-M-," . goto-last-change)
         ("C-M-." . goto-last-change-reverse)))

(use-package visual-regexp :ensure t)
(use-package visual-regexp-steroids :ensure t)

(use-package highlight :ensure t)
(use-package highlight-symbol :ensure t)