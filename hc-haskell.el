;;; hc-haskell --- Summary

;;; Commentary:

;;;;
;;;; Created       : ...                        by Harold Carr.
;;;; Last Modified : 2018 Nov 07 (Wed) 11:28:38 by Harold Carr.
;;;;

;;; Code:

;;; Haskell Packages

;; for Intero, see: https://github.com/cydparser/demo-emacs-haskell/blob/master/demo.org
(use-package haskell-mode
  :ensure t
  :defer t
  :config
  (custom-set-variables
   '(haskell-stylish-on-save t)
   '(haskell-hoogle-command (concat (projectile-project-root) "scripts/hoogle.sh"))
   )

  (defun haskell-hoogle-start-server ()
    "Start hoogle local server."
    (interactive)
      (unless (haskell-hoogle-server-live-p)
      (set 'haskell-hoogle-server-process
	    (start-process
	    haskell-hoogle-server-process-name
	    (get-buffer-create haskell-hoogle-server-buffer-name)
	    haskell-hoogle-command "server" "-p" (number-to-string haskell-hoogle-port-number))))
  )
)


; (use-package haskell-snippets
;   :ensure t
;   :defer t)

;;(use-package hlint-refactor
;;  :ensure t
;;  :defer t
;;  :diminish ""
;;  :init (add-hook 'haskell-mode-hook #'hlint-refactor-mode))

(defvar hc-haskell)

(cond ((y-or-n-p-with-timeout "for Haskell: use Dante/y; Intero/n (the default)" 6 nil)
       (message "Using Dante")
       (setq hc-haskell 'dante)
       (use-package hc-haskell-dante))
      (t
       (message "Using Intero")
       (setq hc-haskell 'intero)
       (use-package hc-haskell-intero)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/knupfer/haskell-emacs

; (use-package haskell-emacs
;   :ensure t
;   :defer t)
; 
; (setq haskell-emacs-build-tool (quote stack))
; (setq haskell-emacs-dir (concat (hcEmacsDir) "/hc-haskell-fun/"))

;;;;;;;;;;;;;

(provide 'hc-haskell)

;;; hc-haskell.el ends here
