(setq scroll-preserve-screen-position 'always)

(use-package ibuffer
  :init (progn
          (setq ibuffer-expert t
                ibuffer-show-empty-filter-groups nil
                ibuffer-formats '((mark modified read-only vc-status-mini " "
                                        (name 18 18 :left :elide) " "
                                        (size-h 9 -1 :right) " "
                                        (mode 16 16 :left :elide) " "
                                        (vc-status 16 16 :left) " "
                                        filename-and-process))
                ibuffer-filter-group-name-face 'font-lock-doc-face)
          (defalias 'list-buffers 'ibuffer))
  :config (progn
            ;; Use human readable Size column instead of original one
            (define-ibuffer-column size-h (:name "Size" :inline t)
              (cond
               ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
               ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
               (t (format "%8d" (buffer-size)))))
            (defun ibuffer-back-to-top ()
              (interactive)
              (beginning-of-buffer)
              (next-line 3))
            (defun ibuffer-jump-to-bottom ()
              (interactive)
              (end-of-buffer)
              (next-line -2))

            (define-key ibuffer-mode-map (vector 'remap 'end-of-buffer) 'ibuffer-jump-to-bottom)
            (define-key ibuffer-mode-map (vector 'remap 'beginning-of-buffer) 'ibuffer-back-to-top)

            (add-hook 'ibuffer-mode-hook
                      (lambda ()
                        (ibuffer-auto-mode 1)
                        (diminish-major-mode 'ibuffer "≣")))))

(use-package ibuffer-vc :ensure t
  :config (progn
            (defun ibuffer-set-up-preferred-filters ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'filename/process)
                (ibuffer-do-sort-by-filename/process)))

            (add-hook 'ibuffer-hook 'ibuffer-set-up-preferred-filters)))

(use-package buffer-move :ensure t
  :bind (("<S-up>"    . buf-move-up)
         ("<S-down>"  . buf-move-down)
         ("<S-left>"  . buf-move-left)
         ("<S-right>" . buf-move-right)
         ("<up>"    . windmove-up)
         ("<down>"  . windmove-down)
         ("<left>"  . windmove-left)
         ("<right>" . windmove-right)))

(use-package transpose-frame :ensure t
  :bind (("s-(" . rotate-frame-anticlockwise)
         ("s-)" . rotate-frame-clockwise)))

(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      mouse-wheel-scroll-amount '(3)
      mouse-wheel-progressive-speed nil)

;; bugs: line doesn't appear or disappear when your point is not in the window.
(use-package fill-column-indicator :ensure t
  :init (progn
          (setq fci-handle-truncate-lines nil
                truncate-lines nil
                truncate-partial-width-windows nil)
          (setq-default fci-rule-column 80))
  :config (progn
            (defun toggle-fci (&optional unused)
              (if (> (window-width) fci-rule-column)
                  (fci-mode 1)
                (fci-mode 0)))

            (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
            (global-fci-mode 1)
            (add-hook 'window-configuration-change-hook 'toggle-fci))
  :bind (("C-x |" . toggle-fci2)))
