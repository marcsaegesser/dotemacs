;;----------------------------------------------------------------------------
;; Multiple major modes
;;----------------------------------------------------------------------------
;; (require-package 'mmm-mode)
;; (require 'mmm-auto)
;; (setq mmm-global-mode 'buffers-with-submode-classes)
;; (setq mmm-submode-decoration-level 2)
(use-package mmm-mode
  :ensure t
  :defer t
  :config
  (setq mmm-global-mode 'buffers-with-submode-classes)
  (setq mmm-submode-decoration-level 2))

(provide 'init-mmm)
