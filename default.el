;;; default.el --- Default settings

;;; Commentary:
;;; Code:

;; globals`

(package-initialize)
;; me me me
(setq user-full-name "Toan Nguyen"
      user-mail-address "hgiasac@gmail.com"
      debug-on-error t
      )

;; set autosave directory
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs-saves/" t)))

					; initialize package
;; (when (memq window-system '(mac ns x))
;;   (exec-path-from-shell-initialize))

(eval-when-compile
  (require 'use-package))

(use-package my-keybinding)

(use-package my-core)
;;; theme
(use-package monokai-theme
  :load-path "themes"
  :config
  (load-theme 'monokai t)
  )

					; loooks pretty good butt.. another time
					; https://github.com/lassik/emacs-format-all-the-code
(use-package format-all ;;
					; -- the haskell mode hook jumps to the top of screen on save
					; :hook (haskell-mode . format-all-mode) ; TODO fixed in https://github.com/lassik/emacs-format-all-the-code/issues/23
  :commands (
	     format-all-mode
	     format-all-buffer
	     )
  )

(use-package flx)
(use-package ranger
  :commands (ranger)
  :config
  (setq
   ranger-cleanup-early t
   ranger-parent-depth 0
   ranger-max-preview-size 1
   ranger-dont-show-binary t
   ranger-preview-delay 0.040
   ranger-excluded-extensions '("tar.gz" "mkv" "iso" "mp4")
   )
  )

;;; show what keys are possible
(use-package which-key
  :config
  (setq which-key-idle-delay 0.01)
  (which-key-mode)
  )

;;; jump around
(use-package avy
  :commands (avy-goto-word-1 avy-goto-word-or-subword-1))
;; more info
(use-package powerline
  :config
  (powerline-default-theme)
  )

;;; nix syntax highlighting
(use-package nix-mode
  :after company
  )

(use-package yaml-mode
  :mode "\\.yaml\\'")
(use-package markdown-mode
  :mode "\\.md\\'")

;;; JS
(use-package rjsx-mode
					; maybe this should work:
					; :mode ("\\.js\\" . rjsx-mode)
  )
					; but no this instead:
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

;; rule 80 chars, if issues: https://github.com/company-mode/company-mode/issues/180#issuecomment-55047120
;; https://emacs.stackexchange.com/questions/147/how-can-i-get-a-ruler-at-column-80
(use-package fill-column-indicator
  :hook (prog-mode . turn-on-fci-mode)
  :config
					; (setq fci-rule-color "white")
  (setq fci-rule-width 2)
  )

;;; `rainbow-delimiters':
;; This is fairly straight forward, it matches pairs of parens with colors,
;; making it easier to at a glance see blocks of code.
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :ghook ('prog-mode-hook #'rainbow-delimiters-mode))

;;; `aggressive-indent':
;; The default indentation mode for Emacs is okay, but when editing LISP you can
;; do so much more. Since it's not whitespace sensitive you're free to
;; manipulate it at will with packages like `smartparens' or `lispy'. This minor
;; mode aggressively indents code whenever you change any part of a code block.
(use-package aggressive-indent
  :delight
  :ghook ('emacs-lisp-mode-hook #'aggressive-indent-mode))

;;; Git
(use-package my-git)
;;; Default Completion
(use-package my-completion)

;;; Haskell
(use-package hc-haskell)

;;; Projectile
(use-package my-projects)

;;; Evil mode
(use-package am-evil)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; default.el ends here
