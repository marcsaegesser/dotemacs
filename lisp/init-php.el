(use-package flymake-php :ensure t)

(use-package php-mode
  :ensure t
  :hook (flymake-php-load)
  )

;; (require-package 'php-mode)
;; (require-package 'smarty-mode)
;; (require-package 'flymake-php)

;; (add-hook 'php-mode-hook 'flymake-php-load)

(provide 'init-php)
