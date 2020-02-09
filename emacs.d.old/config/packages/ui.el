(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1)
  (menu-bar-mode 1)
  (desktop-save-mode 1))

(use-package rainbow-mode :ensure t
  :diminish rainbow-mode)

(use-package popwin :ensure t
  :init (setq display-buffer-function 'popwin:display-buffer
              popwin:special-display-config
              '(("*Help*"  :height 30 :stick t)
                ("*Completions*" :noselect t)
                ("*compilation*" :noselect t)
                ("*Messages*" :height 30)
                ("*Occur*" :noselect t)
                ("*sldb.*":regexp t :height 30)
                ("*magit-commit*" :noselect t :height 40 :width 80)
                ("*magit-diff*" :noselect t :height 40 :width 80)
                ("*magit-edit-log*" :noselect t :height 15 :width 80)
                ("*Ido Completions*" :noselect t :height 30)
                ("*eshell*" :height 30)
                ("*shell*" :height 30)
                ("\\*ansi-term\\*.*" :regexp t :height 30)
                (".*overtone.log" :regexp t :height 30)
                ("*gists*" :height 30)
                ("*Kill Ring*" :height 30)
                ("*Compile-Log*" :height 30)
                (" *auto-async-byte-compile*" :height 14 :position bottom)
                ("*VC-log*" :height 10 :position bottom)
                ("*nREPL doc*" :height 30 :position bottom)
                ("*nREPL error*" :height 30 :position bottom)
                ("*nREPL inspect*" :height 20 :position bottom)
                ("*nREPL Macroexpansion*" :height 30 :position bottom)
                ("nREPL-tmp" :height 30 :position bottom))))

(defun my-terminal-visible-bell ()
  "A friendlier visual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))
(setq visible-bell nil ring-bell-function 'my-terminal-visible-bell)
