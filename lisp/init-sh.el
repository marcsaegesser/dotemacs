(use-package flymake-shell
  :ensure t
  :hook (sh-set-shell-hook . flymake-shell-load)
  )

;; (require-package 'flymake-shell)
;; (add-hook 'sh-set-shell-hook 'flymake-shell-load)


(provide 'init-sh)
