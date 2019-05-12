;; globals`
(set-default 'truncate-lines t)
(setq-default indent-tabs-mode nil) ;; disable tabs
(setq tab-width 2)
(setq version-control t )		; use version control
(setq vc-follow-symlinks t )				       ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
(setq inhibit-startup-screen t )	; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore )	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8 )	; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
(setq default-fill-column 90)		; toggle wrapping text at the 80th character
(setq initial-scratch-message "Good day sir, your wish is my command.") ; Emacs shows its subservience. Machines are tools.


;; backup https://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
(setq vc-make-backup-files t)
(setq kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.
;; Default and per-save backups go here:
(setq backup-directory-alist '(("" . "~/.emacs.d/backup/per-save")))

(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.emacs.d/backup/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))
(add-hook 'before-save-hook  'force-backup-of-buffer)

;; font
(push '(font . "firacode-12") default-frame-alist)
;;; Fira code
;; This works when using emacs --daemon + emacsclient
(add-hook 'after-make-frame-functions (lambda (frame) (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")))
;; This works when using emacs without server/client
;(set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")
;; I haven't found one statement that makes both of the above situations work, so I use both for now

(defconst fira-code-font-lock-keywords-alist
  (mapcar (lambda (regex-char-pair)
            `(,(car regex-char-pair)
              (0 (prog1 ()
                   (compose-region (match-beginning 1)
                                   (match-end 1)
                                   ;; The first argument to concat is a string containing a literal tab
                                   ,(concat "	" (list (decode-char 'ucs (cadr regex-char-pair)))))))))
          '(("\\(www\\)"                   #Xe100)
            ("[^/]\\(\\*\\*\\)[^/]"        #Xe101)
            ("\\(\\*\\*\\*\\)"             #Xe102)
            ("\\(\\*\\*/\\)"               #Xe103)
            ("\\(\\*>\\)"                  #Xe104)
            ("[^*]\\(\\*/\\)"              #Xe105)
            ("\\(\\\\\\\\\\)"              #Xe106)
            ("\\(\\\\\\\\\\\\\\)"          #Xe107)
            ("\\({-\\)"                    #Xe108)
            ("\\(\\[\\]\\)"                #Xe109)
            ("\\(::\\)"                    #Xe10a)
            ("\\(:::\\)"                   #Xe10b)
            ("[^=]\\(:=\\)"                #Xe10c)
            ("\\(!!\\)"                    #Xe10d)
            ("\\(!=\\)"                    #Xe10e)
            ("\\(!==\\)"                   #Xe10f)
            ("\\(-}\\)"                    #Xe110)
            ("\\(--\\)"                    #Xe111)
            ("\\(---\\)"                   #Xe112)
            ("\\(-->\\)"                   #Xe113)
            ("[^-]\\(->\\)"                #Xe114)
            ("\\(->>\\)"                   #Xe115)
            ("\\(-<\\)"                    #Xe116)
            ("\\(-<<\\)"                   #Xe117)
            ("\\(-~\\)"                    #Xe118)
            ("\\(#{\\)"                    #Xe119)
            ("\\(#\\[\\)"                  #Xe11a)
            ("\\(##\\)"                    #Xe11b)
            ("\\(###\\)"                   #Xe11c)
            ("\\(####\\)"                  #Xe11d)
            ("\\(#(\\)"                    #Xe11e)
            ("\\(#\\?\\)"                  #Xe11f)
            ("\\(#_\\)"                    #Xe120)
            ("\\(#_(\\)"                   #Xe121)
            ("\\(\\.-\\)"                  #Xe122)
            ("\\(\\.=\\)"                  #Xe123)
            ("\\(\\.\\.\\)"                #Xe124)
            ("\\(\\.\\.<\\)"               #Xe125)
            ("\\(\\.\\.\\.\\)"             #Xe126)
            ("\\(\\?=\\)"                  #Xe127)
            ("\\(\\?\\?\\)"                #Xe128)
            ("\\(;;\\)"                    #Xe129)
            ("\\(/\\*\\)"                  #Xe12a)
            ("\\(/\\*\\*\\)"               #Xe12b)
            ("\\(/=\\)"                    #Xe12c)
            ("\\(/==\\)"                   #Xe12d)
            ("\\(/>\\)"                    #Xe12e)
            ("\\(//\\)"                    #Xe12f)
            ("\\(///\\)"                   #Xe130)
            ("\\(&&\\)"                    #Xe131)
            ("\\(||\\)"                    #Xe132)
            ("\\(||=\\)"                   #Xe133)
            ("[^|]\\(|=\\)"                #Xe134)
            ("\\(|>\\)"                    #Xe135)
            ("\\(\\^=\\)"                  #Xe136)
            ("\\(\\$>\\)"                  #Xe137)
            ("\\(\\+\\+\\)"                #Xe138)
            ("\\(\\+\\+\\+\\)"             #Xe139)
            ("\\(\\+>\\)"                  #Xe13a)
            ("\\(=:=\\)"                   #Xe13b)
            ("[^!/]\\(==\\)[^>]"           #Xe13c)
            ("\\(===\\)"                   #Xe13d)
            ("\\(==>\\)"                   #Xe13e)
            ("[^=]\\(=>\\)"                #Xe13f)
            ("\\(=>>\\)"                   #Xe140)
            ("\\(<=\\)"                    #Xe141)
            ("\\(=<<\\)"                   #Xe142)
            ("\\(=/=\\)"                   #Xe143)
            ("\\(>-\\)"                    #Xe144)
            ("\\(>=\\)"                    #Xe145)
            ("\\(>=>\\)"                   #Xe146)
            ("[^-=]\\(>>\\)"               #Xe147)
            ("\\(>>-\\)"                   #Xe148)
            ("\\(>>=\\)"                   #Xe149)
            ("\\(>>>\\)"                   #Xe14a)
            ("\\(<\\*\\)"                  #Xe14b)
            ("\\(<\\*>\\)"                 #Xe14c)
            ("\\(<|\\)"                    #Xe14d)
            ("\\(<|>\\)"                   #Xe14e)
            ("\\(<\\$\\)"                  #Xe14f)
            ("\\(<\\$>\\)"                 #Xe150)
            ("\\(<!--\\)"                  #Xe151)
            ("\\(<-\\)"                    #Xe152)
            ("\\(<--\\)"                   #Xe153)
            ("\\(<->\\)"                   #Xe154)
            ("\\(<\\+\\)"                  #Xe155)
            ("\\(<\\+>\\)"                 #Xe156)
            ("\\(<=\\)"                    #Xe157)
            ("\\(<==\\)"                   #Xe158)
            ("\\(<=>\\)"                   #Xe159)
            ("\\(<=<\\)"                   #Xe15a)
            ("\\(<>\\)"                    #Xe15b)
            ("[^-=]\\(<<\\)"               #Xe15c)
            ("\\(<<-\\)"                   #Xe15d)
            ("\\(<<=\\)"                   #Xe15e)
            ("\\(<<<\\)"                   #Xe15f)
            ("\\(<~\\)"                    #Xe160)
            ("\\(<~~\\)"                   #Xe161)
            ("\\(</\\)"                    #Xe162)
            ("\\(</>\\)"                   #Xe163)
            ("\\(~@\\)"                    #Xe164)
            ("\\(~-\\)"                    #Xe165)
            ("\\(~=\\)"                    #Xe166)
            ("\\(~>\\)"                    #Xe167)
            ("[^<]\\(~~\\)"                #Xe168)
            ("\\(~~>\\)"                   #Xe169)
            ("\\(%%\\)"                    #Xe16a)
           ;; ("\\(x\\)"                   #Xe16b) This ended up being hard to do properly so i'm leaving it out.
            ("[^:=]\\(:\\)[^:=]"           #Xe16c)
            ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d)
            ("[^\\*/<>]\\(\\*\\)[^\\*/<>]" #Xe16f))))

(defun add-fira-code-symbol-keywords ()
  (font-lock-add-keywords nil fira-code-font-lock-keywords-alist))

(add-hook 'prog-mode-hook
          #'add-fira-code-symbol-keywords)

;; me me me
(setq user-full-name "Toan Nguyen"
      user-mail-address "hgiasac@gmail.com")

;; use windows logo as meta, alt is used by i3
(setq x-super-keysym 'meta) 

;; Annoying random freezes
(setq x-select-enable-clipboard-manager nil)

;;; I'm not a mouse peasant (disable menu/toolbars)
(tool-bar-mode -1) ;; disables tool buttons (little icons)
(menu-bar-mode -1) ;; disables file edit help etc
(scroll-bar-mode -1) ;; disables scrol bar

(global-hl-line-mode +1) ;; highlight current line

;; initialize package
(eval-when-compile
  (require 'use-package))

;;; theme
(use-package monokai-theme
   :load-path "themes"
   :config
  (load-theme 'monokai t)
)

;;; keybindings
(use-package general
  :config
  (general-define-key "C-'" 'avy-goto-word-1)
  (general-define-key "C-x b" 'ivy-switch-buffer)
  (general-define-key
      :keymaps 'normal
        ;; simple command
        "K" 'newline
        )
  (general-define-key
    ;; replace default keybindings
    "C-s" 'swiper             ; search for string in current buffer
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
)

;;; project navigation
(use-package counsel-projectile
  :commands (
    counsel-projectile-find-file
    counsel-projectile-rg
    counsel-projectile
    counsel-projectile-ag
    )
  :config
  (counsel-projectile-mode)
  )

(use-package projectile
  :config
  (setq projectile-enable-caching nil)
  (projectile-mode) ;; I always want this?
  ;; https://emacs.stackexchange.com/questions/16497/how-to-exclude-files-from-projectile
  ;;; Default rg arguments
  ;; https://github.com/BurntSushi/ripgrep
  (when (executable-find "rg")
    (progn
      (defconst modi/rg-arguments
        `("--line-number"                     ; line numbers
          "--smart-case"
          "--follow"                          ; follow symlinks
          "--mmap")                           ; apply memory map optimization when possible
        "Default rg arguments used in the functions in `projectile' package.")

      (defun modi/advice-projectile-use-rg ()
        "Always use `rg' for getting a list of all files in the project."
        (mapconcat 'identity
                   (append '("\\rg") ; used unaliased version of `rg': \rg
                           modi/rg-arguments
                           '("--null" ; output null separated results,
                             "--files")) ; get file names matching the regex '' (all files)
                   " "))
      (advice-add 'projectile-get-ext-command :override #'modi/advice-projectile-use-rg)))
)
(use-package swiper
  :commands (
    swiper
    ))

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

(use-package ivy
  :after flx
  :commands (ivy-switch-buffer)
  :config
    (setq ivy-re-builders-alist
        '((ivy-switch-buffer . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
    (setq ivy-initial-inputs-alist nil)
)

(use-package ranger
  :commands (ranger)
  :config
  (setq
    ranger-cleanup-eagerly t
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

;;; git
(use-package magit
  :defer
  :commands (magit-status magit-dispatch-popup magit-push-to-remote)
  )

;;; I can't spell
(use-package flycheck
  :defer 2
  :config (global-flycheck-mode))

;;; I can't program
(use-package company
  :diminish company-mode
  :commands (company-mode global-company-mode)
  :defer 1
  :config
  (global-company-mode))

;;; more info
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

;;; Haskell
(use-package 'hc-haskell)
;;; use emacs as mergetool
(defvar ediff-after-quit-hooks nil
  "* Hooks to run after ediff or emerge is quit.")

(defadvice ediff-quit (after edit-after-quit-hooks activate)
  (run-hooks 'ediff-after-quit-hooks))

(setq git-mergetool-emacsclient-ediff-active nil)


(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(defun local-ediff-before-setup-hook ()
  (setq local-ediff-saved-frame-configuration (current-frame-configuration))
  (setq local-ediff-saved-window-configuration (current-window-configuration))
  ;; (local-ediff-frame-maximize)
  (if git-mergetool-emacsclient-ediff-active
      (raise-frame)))

(defun local-ediff-quit-hook ()
  (set-frame-configuration local-ediff-saved-frame-configuration)
  (set-window-configuration local-ediff-saved-window-configuration))

(defun local-ediff-suspend-hook ()
  (set-frame-configuration local-ediff-saved-frame-configuration)
  (set-window-configuration local-ediff-saved-window-configuration))

(add-hook 'ediff-before-setup-hook 'local-ediff-before-setup-hook)
(add-hook 'ediff-quit-hook 'local-ediff-quit-hook 'append)
(add-hook 'ediff-suspend-hook 'local-ediff-suspend-hook 'append)

;; Useful for ediff merge from emacsclient.
(defun git-mergetool-emacsclient-ediff (local remote base merged)
  (setq git-mergetool-emacsclient-ediff-active t)
  (if (file-readable-p base)
      (ediff-merge-files-with-ancestor local remote base nil merged)
    (ediff-merge-files local remote nil merged))
  (recursive-edit))

(defun git-mergetool-emacsclient-ediff-after-quit-hook ()
  (exit-recursive-edit))

(add-hook 'ediff-after-quit-hooks 'git-mergetool-emacsclient-ediff-after-quit-hook 'append)

(defun insert-date (prefix)
"Insert the current date. With prefix-argument, use ISO format. With
two prefix arguments, write out the day and month name."
(interactive "P")
(let ((format (cond
                ((not prefix) "%d.%m.%Y")
                ((equal prefix '(4)) "%Y-%m-%d")
                ((equal prefix '(16)) "%A, %d. %B %Y")))
        (system-time-locale "de_DE"))
    (insert (format-time-string format))))


;; rule 80 chars, if issues: https://github.com/company-mode/company-mode/issues/180#issuecomment-55047120
;; https://emacs.stackexchange.com/questions/147/how-can-i-get-a-ruler-at-column-80
(use-package fill-column-indicator
  :hook (prog-mode . turn-on-fci-mode)
  :config
  ; (setq fci-rule-color "white")
  (setq fci-rule-width 2)
)

