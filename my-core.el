;;; my-core.el --- Core Settings -*- lexical-binding: t -*-

;;; Commentary:

;; Core configuration.

;;; Core settings
(defmacro csetq (&rest body)
  "A `setq' macro that works with `custom-set' properties. The
BODY is a list of the variables to be set."
  `(progn
     ,@(cl-loop for (var val) on body by 'cddr
                collect `(funcall (or (get ',var 'custom-set) #'set)
                                  ',var ,val))))

(setq x-super-keysym 'meta)

;; Annoying random freezes
(setq x-select-enable-clipboard-manager nil)

;;; I'm not a mouse peasant (disable menu/toolbars)
(tool-bar-mode -1) ;; disables tool buttons (little icons)
(menu-bar-mode -1) ;; disables file edit help etc
(scroll-bar-mode -1) ;; disables scrol bar

(global-hl-line-mode +1) ;; highlight current line

;;; `paren':
;; Does pretty much exactly what it says, it shows matching parenthesizes (and
;; other delimiters as far as I'm aware too). As for settings, we'll set it so
;; there's no delay for showing it's long lost sister, always highlight open
;; parenthesises and show the matching pair when inside their block.
(use-package paren
  :commands (show-paren-mode)
  :init (show-paren-mode)
  :config
  (progn
    (setq-default show-paren-delay 0                      ;; Show matching parenthesis without delay.
                  show-paren-highlight-openparen t        ;; Always show the matching parenthesis.
                  show-paren-when-point-inside-paren t))) ;; Show parenthesis when inside a block.
                                        ;

(provide 'my-core)

;;; my-core.el ends here
