;; Use C-f during file selection to switch to regular find-file

(use-package ido
  :init
  (setq ido-enable-flex-matching t)
  (setq ido-use-filename-at-point nil)
  (setq ido-auto-merge-work-directories-length 0)
  (setq ido-use-virtual-buffers t)
  (setq ido-default-buffer-method 'selected-window)
  :config
  (ido-mode t)
  (ido-everywhere t))

(use-package ido-vertical-mode
  :ensure t
  :init
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  :config
  (ido-vertical-mode t))

(use-package flx-ido
  :ensure t
  :config
  (flx-ido-mode t))

(use-package ido-completing-read+
  :ensure t
  :config
  (ido-ubiquitous-mode t))

(use-package smex
  :ensure t
  :bind ("M-x" . 'smex)
  )

;; (setq ido-enable-flex-matching t)
;; (setq ido-use-filename-at-point nil)
;; (setq ido-auto-merge-work-directories-length 0)
;; (setq ido-use-virtual-buffers t)
;; (setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; (require-package 'ido-ubiquitous)
;; (ido-ubiquitous-mode t)

;; (require-package 'smex)
;; (global-set-key (kbd "M-x") 'smex)

;; (require-package 'idomenu)

;; Allow the same buffer to be open in different frames
;; (setq ido-default-buffer-method 'selected-window)

;; (when (eval-when-compile (< emacs-major-version 24))
;;  (defun sanityinc/ido-choose-from-recentf ()
;;    "Use ido to select a recently opened file from the `recentf-list'"
;;    (interactive)
;;    (if (and ido-use-virtual-buffers (fboundp 'ido-toggle-virtual-buffers))
;;        (ido-switch-buffer)
;;      (find-file (ido-completing-read "Open file: " recentf-list nil t))))

;;  (global-set-key [(meta f11)] 'sanityinc/ido-choose-from-recentf))



(provide 'init-ido)
