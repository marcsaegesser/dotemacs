;;----------------------------------------------------------------------------
;; Navigate window layouts with "C-c <left>" and "C-c <right>"
;;----------------------------------------------------------------------------
(winner-mode 1)



;; Make "C-x o" prompt for a target window when there are more than 2
;; (require-package 'switch-window)
;; (require 'switch-window)
;; (setq switch-window-shortcut-style 'alphabet)
;; (global-set-key "\C-xo" 'switch-window)
(use-package switch-window
  :ensure t
  :config
  (setq switch-window-shortcut-style 'alphabet)
  :bind
  (("\C-xo" . switch-window)
   ("C-x 9" . switch-window-then-delete)))

;;----------------------------------------------------------------------------
;; When splitting window, show (other-buffer) in the new window
;;----------------------------------------------------------------------------
;; Note: Maybe put this back after experimenting. mas 2/6/2018
;; (defun split-window-func-with-other-buffer (split-function)
;;   (lexical-let ((s-f split-function))
;;     (lambda ()
;;       (interactive)
;;       (funcall s-f)
;;       (set-window-buffer (next-window) (other-buffer)))))

;; (global-set-key "\C-x2" (split-window-func-with-other-buffer 'split-window-vertically))
;; (global-set-key "\C-x3" (split-window-func-with-other-buffer 'split-window-horizontally))


;;----------------------------------------------------------------------------
;; Rearrange split windows
;;----------------------------------------------------------------------------
;; (defun split-window-horizontally-instead ()
;;   (interactive)
;;   (save-excursion
;;     (delete-other-windows)
;;     (funcall (split-window-func-with-other-buffer 'split-window-horizontally))))

;; (defun split-window-vertically-instead ()
;;   (interactive)
;;   (save-excursion
;;     (delete-other-windows)
;;     (funcall (split-window-func-with-other-buffer 'split-window-vertically))))

;; (global-set-key "\C-x|" 'split-window-horizontally-instead)
;; (global-set-key "\C-x_" 'split-window-vertically-instead)


(provide 'init-windows)
