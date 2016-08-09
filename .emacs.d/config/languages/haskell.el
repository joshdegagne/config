(setenv "PATH" (shell-command-to-string "echo $PATH"))

;; structured-haskell-mode --------------
(package-install 'haskell-mode)
(package-install 'shm)
(require 'haskell-mode)
(require 'haskell-interactive-mode)
(require 'haskell-process)
(setq shm-program-name "/Users/joshua.degagne/.cabal/bin/structured-haskell-mode")
(load "haskell-mode-autoloads")
(require 'shm)

;; THIS SPITS CODE INTO: init.el
(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 ;; Use cabal-dev for the GHCi session. Ensures our dependencies are in scope.
 '(haskell-process-type 'cabal-dev)
 ;; Use notify.el (if you have it installed) at the end of running
 ;; Cabal commands or generally things worth notifying.
 '(haskell-notify-p t)
 ;; To enable tags generation on save.
 '(haskell-tags-on-save t)
 ;; To enable stylish on save.
 '(haskell-stylish-on-save t)
 ;; colours
 (set-face-background 'shm-current-face "#eee8d5")
 (set-face-background 'shm-quarantine-face "lemonchiffon"))

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'structured-haskell-mode)
(define-key shm-map (kbd "C-c C-s") 'shm/case-split)
