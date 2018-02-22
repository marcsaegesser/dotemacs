(use-package paredit
  :ensure t
  :diminish
  :hook ((lisp-mode emacs-lisp-mode) . paredit-mode)
  :bind (:map paredit-mode-map
              ("[")
              ("M-k"   . paredit-raise-sexp)
              ("M-I"   . paredit-splice-sexp)
              ("C-M-l" . paredit-recentre-on-sexp)
              ("C-c ( n"   . paredit-add-to-next-list)
              ("C-c ( p"   . paredit-add-to-previous-list)
              ("C-c ( j"   . paredit-join-with-next-list)
              ("C-c ( J"   . paredit-join-with-previous-list))
  :bind (:map lisp-mode-map       ("<return>" . paredit-newline))
  :bind (:map emacs-lisp-mode-map ("<return>" . paredit-newline))
  :hook (paredit-mode
         . (lambda ()
             (unbind-key "M-r" paredit-mode-map)
             (unbind-key "M-s" paredit-mode-map)))
  :config
  (require 'eldoc)
  (eldoc-add-command 'paredit-backward-delete
                     'paredit-close-round))


;; (require-package 'paredit)
;; (autoload 'enable-paredit-mode "paredit")

;; (defun maybe-map-paredit-newline ()
;;   (unless (or (memq major-mode '(inferior-emacs-lisp-mode nrepl-mode))
;;               (minibufferp))
;;     (local-set-key (kbd "RET") 'paredit-newline)))

;; (add-hook 'paredit-mode-hook 'maybe-map-paredit-newline)

;; (eval-after-load 'paredit
;;   '(progn
;;      (diminish 'paredit-mode " Par")
;;      (dolist (binding (list (kbd "C-<left>") (kbd "C-<right>")
;;                             (kbd "C-M-<left>") (kbd "C-M-<right>")))
;;        (define-key paredit-mode-map binding nil))

;;      ;; Disable kill-sentence, which is easily confused with the kill-sexp
;;      ;; binding, but doesn't preserve sexp structure
;;      (define-key paredit-mode-map [remap kill-sentence] nil)
;;      (define-key paredit-mode-map [remap backward-kill-sentence] nil)))


;; ;; Compatibility with other modes

;; ;; (suspend-mode-during-cua-rect-selection 'paredit-mode)


;; ;; Use paredit in the minibuffer
;; (add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

;; (defvar paredit-minibuffer-commands '(eval-expression
;;                                       pp-eval-expression
;;                                       eval-expression-with-eldoc
;;                                       ibuffer-do-eval
;;                                       ibuffer-do-view-and-eval)
;;   "Interactive commands for which paredit should be enabled in the minibuffer.")

;; (defun conditionally-enable-paredit-mode ()
;;   "Enable paredit during lisp-related minibuffer commands."
;;   (if (memq this-command paredit-minibuffer-commands)
;;       (enable-paredit-mode)))

;; (global-set-key (kbd "M-[") 'paredit-wrap-square)

;; ----------------------------------------------------------------------------
;; Enable some handy paredit functions in all prog modes
;; ----------------------------------------------------------------------------

(use-package paredit-everywhere
  :ensure t
  :hook (prog-mode . paredit-everywhere-mode))

;; (require-package 'paredit-everywhere)
;; (add-hook 'prog-mode-hook 'paredit-everywhere-mode)

(provide 'init-paredit)
