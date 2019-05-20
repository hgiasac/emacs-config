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


(provide 'my-projects)

;;; my-projects ends here

