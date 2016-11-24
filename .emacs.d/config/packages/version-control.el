(use-package diff-mode
  :config (progn (setq diff-switches "-u")
                 (set-face-foreground 'diff-added "green4")
                 (set-face-foreground 'diff-removed "red3")))

(use-package magit :ensure t
  :init (progn (setq magit-last-seen-setup-instructions "1.4.0")
               (setq magit-display-buffer-function
                     #'magit-display-buffer-fullframe-status-v1)
               (setq-default magit-save-some-buffers nil
                             magit-process-popup-time 10
                             magit-diff-refine-hunk t
                             magit-completing-read-function 'magit-ido-completing-read))
  :bind ("C-x m" . magit-status)
  :config (progn
            (defun magit-ignore-whitespace ()
              (interactive)
              (add-to-list 'magit-diff-options "-w")
              (magit-refresh))

            (defun magit-dont-ignore-whitespace ()
              (interactive)
              (setq magit-diff-options (remove "-w" magit-diff-options))
              (magit-refresh))

            (defun magit-toggle-whitespace ()
              (interactive)
              (if (member "-w" magit-diff-options)
                  (magit-dont-ignore-whitespace)
                (magit-ignore-whitespace)))

            (bind-key "C-x C-k" 'magit-kill-file-on-line magit-status-mode-map)
            (bind-key "W" 'magit-toggle-whitespace magit-status-mode-map)))

(defadvice vc-annotate (around fullscreen activate)
  (window-configuration-to-register :vc-annotate-fullscreen)
  ad-do-it
  (delete-other-windows))

(use-package vc-annotate
  :init (defun vc-annotate-quit ()
          "Restores the previous window configuration and kills the vc-annotate buffer"
          (interactive)
          (kill-buffer)
          (jump-to-register :vc-annotate-fullscreen))
  :config (bind-key "q" 'vc-annotate-quit vc-annotate-mode-map))
