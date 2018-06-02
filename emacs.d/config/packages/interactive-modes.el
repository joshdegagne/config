(use-package dired
  :init (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  :bind ("C-x C-d" . dired)
  :config (define-key dired-mode-map (kbd "C-x C-k") 'dired-do-delete))

;;(use-package dired+ :ensure t :config (diredp-toggle-find-file-reuse-dir 1))

(use-package shell-pop :ensure t
  :config (custom-set-variables
           '(shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell shell-pop-term-shell)))))
           '(shell-pop-universal-key "M-`")
           '(shell-pop-window-height 100)
           '(shell-pop-window-position "top")))

(use-package ido-completing-read+ :ensure t
  :config (progn
            (defun ido-imenu ()
              "Update the imenu index and then use ido to select a symbol to navigate to.
Symbols matching the text at point are put first in the completion list."
              (interactive)
              (imenu--make-index-alist)
              (let ((name-and-pos '())
                    (symbol-names '()))
                (flet ((addsymbols (symbol-list)
                                   (when (listp symbol-list)
                                     (dolist (symbol symbol-list)
                                       (let ((name nil) (position nil))
                                         (cond
                                          ((and (listp symbol) (imenu--subalist-p symbol))
                                           (addsymbols symbol))

                                          ((listp symbol)
                                           (setq name (car symbol))
                                           (setq position (cdr symbol)))

                                          ((stringp symbol)
                                           (setq name symbol)
                                           (setq position (get-text-property 1 'org-imenu-marker symbol))))

                                         (unless (or (null position) (null name))
                                           (add-to-list 'symbol-names name)
                                           (add-to-list 'name-and-pos (cons name position))))))))
                      (addsymbols imenu--index-alist))
                ;; If there are matching symbols at point, put them at the beginning of `symbol-names'.
                (let ((symbol-at-point (thing-at-point 'symbol)))
                  (when symbol-at-point
                    (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
                           (matching-symbols (delq nil (mapcar (lambda (symbol)
                                                                 (if (string-match regexp symbol) symbol))
                                                               symbol-names))))
                      (when matching-symbols
                        (sort matching-symbols (lambda (a b) (> (length a) (length b))))
                        (mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
                              matching-symbols)))))
                (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
                       (position (cdr (assoc selected-symbol name-and-pos))))
                  (push-mark (point))
                  (goto-char position))))
            (bind-key "C-c C-i" 'ido-imenu)

            (ido-mode t)
            (ido-ubiquitous-mode)
            (ido-everywhere t)
            (add-to-list 'ido-ignore-files "\\.DS_Store")
            (add-hook 'ido-setup-hook
                      (lambda ()
                        ;; Go straight home
                        (define-key ido-file-completion-map (kbd "~")
                          (lambda ()
                            (interactive)
                            (if (looking-back "/")
                                (insert "~/")
                              (call-interactively 'self-insert-command))))))))

(use-package flx-ido :ensure t
  :init (setq ido-use-faces nil) ; disable ido faces to see flx highlights
  :config (flx-ido-mode t))

(setq ido-auto-merge-delay-time 9) ; unit is seconds

(use-package smex :ensure t
  :init (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  :bind (("M-x" . smex)
         ("C-x C-m" . smex)))
