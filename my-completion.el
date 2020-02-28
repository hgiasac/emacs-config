;;; my-completion --- Completion module

;;; Commentary:

;;; Code:

(use-package ivy
  :ensure t
  :after flx
  :commands (ivy-switch-buffer)
  :config
  (setq ivy-re-builders-alist
        '((ivy-switch-buffer . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil)
  )


;;; `Counsel':
;; Counsel is built on top of Ivy and contains a bunch of improved interfaces
;; for mechanisms in Emacs, like finding files or opening recent files etc.
(use-package counsel
  :ensure t
  :demand t
  :delight
  :general
  (:keymaps 'ivy-mode-map
            [remap find-file]                'counsel-find-file
            [remap recentf]                  'counsel-recentf
            [remap imenu]                    'counsel-imenu
            [remap bookmark-jump]            'counsel-bookmark
            [remap execute-extended-command] 'counsel-M-x
            [remap describe-function]        'counsel-describe-function
            [remap describe-variable]        'counsel-describe-variable
            [remap describe-face]            'counsel-describe-face
            [remap eshell-list-history]      'counsel-esh-history)
    (amalthea-leader
    "a u" '(counsel-unicode-char :wk "find unicode symbol")
    "b b" '(ivy-switch-buffer :wk "change buffer")
    "f f" '(find-file :wk "find file")
    "f r" '(recentf :wk "find recent")
    "f s" '(save-buffer :wk "save buffer")))



;;; `Swiper':
;; This is just a straight upgrade of the default search in Emacs. Use it and
;; love it.
(use-package swiper
  :ensure t
  :general
  (general-define-key "C-s" 'swiper)
  (general-nmap "/" 'swiper)
  (amalthea-leader "/" 'swiper))

;;; I can't spell
(use-package flycheck
  :ensure t
  :defer 2
  :init
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  :config
  (global-flycheck-mode)
  )

;;; I can't program
(use-package company
  :ensure t
  :diminish company-mode
  :commands (company-mode global-company-mode)
  :defer 1
  :config
  (global-company-mode))


;;; `company-quickhelp':
;; When idling on a chosen completion candidate, show the items help in a popup
;; box next to the completion window.
(use-package company-quickhelp
  :ensure t
  :after company
  :commands company-quickhelp-mode
  :init (csetq company-quickhelp-use-propertized-text t) ;; Allow text to have properties like size, color etc
  :config (company-quickhelp-mode))

(provide 'my-completion)

;; my-completion ends here
