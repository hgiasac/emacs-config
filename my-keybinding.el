;;; my-keybinding --- Key-binding settings

;;; Commentary:

;;; Code:

(use-package general
  :ensure t)
(general-define-key "C-'" 'avy-goto-word-1)
(general-define-key
 :keymaps 'normal
 ;; simple command
 "K" 'newline
 )
(general-define-key
 ;; replace default keybindings
 "M-x" 'counsel-M-x        ; replace default M-x with ivy backend
 )
(general-define-key
 :keymaps '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "C-SPC"

 ;; simple command
 "/"   'counsel-projectile-rg
 "k"   '(projectile-kill-buffers :which-key "kill project buffers") ;; sometimes projectile gets confused about temp files, this fixes that
 "SPC" '(avy-goto-word-or-subword-1  :which-key "go to char")
 "b"	'ivy-switch-buffer  ; change buffer, chose using ivy
 ;; bind to double key press
 "j"  'xref-find-definitions ; lsp find definition
 "l"  'counsel-list-processes
 "f"   '(:ignore t :which-key "find/format")
 "ff"  'format-all-buffer
 "fi"  'counsel-projectile-find-file
 "fr"  'projectile-replace-regexp
 "fg"  'counsel-git-grep
 "fh"  'haskell-hoogle-lookup-from-local
 "fa"  'counsel-projectile-ag
 "f/"  'counsel-projectile-rg ; dumb habit
 "h"   '(:ignore t :which-key "hoogle/inspection")
 "hl"  'haskell-hoogle-lookup-from-local
 "hq"  'haskell-hoogle
 "s"  'save-some-buffers
 "p"  'counsel-projectile
 "r"	 'counsel-recentf
 "q"   'kill-emacs
 "g"   '(:ignore t :which-key "git")
 "gg"  'counsel-git-grep
 "gf"  '(counsel-git :which-key "find file in git dir")
 "gs"  'magit-status
 "gp"  'magit-push-to-remote
 "gb"  'magit-blame
 ;; Applications
 "a" '(:ignore t :which-key "Applications")
 "d" 'insert-date
 ";" 'comment-line
 "ar" 'ranger)

(provide 'my-keybinding)
;;; my-keybinding.el ends here
