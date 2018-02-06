(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (add-to-list 'flycheck-disabled-checkers #'javascript-jshint)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-mode 1))

(use-package js3-mode
  ;; jww (2017-12-10): Need to configure.
  :disabled t)

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(use-package json-reformat
  :ensure t
  :after json-mode)

(use-package json-snatcher
  :ensure t
  :after json-mode)



;; (require-package 'json)
;; (require-package 'js3-mode)
;; (when (>= emacs-major-version 24)
;;   (require-package 'js2-mode))
;; (require-package 'js-comint)
;; (require-package 'rainbow-delimiters)
;; (require-package 'coffee-mode)
;; (require-package 'flymake-coffee)
;; (require-package 'flymake-jslint)
;; (require-package 'flymake-json)


;; (defcustom preferred-javascript-mode
;;   (first (remove-if-not #'fboundp '(js2-mode js3-mode)))
;;   "Javascript mode to use for .js files."
;;   :type 'symbol
;;   :group 'programming
;;   :options '(js2-mode js3-mode js-mode))
;; (defvar preferred-javascript-indent-level 2)

;; ;; Need to first remove from list if present, since elpa adds entries too, which
;; ;; may be in an arbitrary order
;; (eval-when-compile (require 'cl))
;; (setq auto-mode-alist (cons `("\\.js\\(\\.erb\\)?\\'" . ,preferred-javascript-mode)
;;                             (loop for entry in auto-mode-alist
;;                                   unless (eq preferred-javascript-mode (cdr entry))
;;                                   collect entry)))


;; (add-auto-mode 'js-mode "\\.json\\'")
;; (add-hook 'js-mode-hook 'flymake-json-maybe-load)

;; ;; On-the-fly syntax checking
;; (eval-after-load 'js
;;   '(add-hook 'js-mode-hook 'flymake-jslint-load))


;; ;; js2-mode
;; (add-hook 'js2-mode-hook '(lambda () (setq mode-name "JS2")))
;; (setq js2-use-font-lock-faces t
;;       js2-mode-must-byte-compile nil
;;       js2-basic-offset preferred-javascript-indent-level
;;       js2-indent-on-enter-key t
;;       js2-auto-indent-p t
;;       js2-bounce-indent-p nil)

;; (eval-after-load 'js2-mode '(js2-imenu-extras-setup))

;; ;; js3-mode
;; (add-hook 'js3-mode-hook '(lambda () (setq mode-name "JS3")))
;; (setq js3-auto-indent-p t
;;       js3-enter-indents-newline t
;;       js3-indent-on-enter-key t
;;       js3-indent-level preferred-javascript-indent-level)

;; ;; js-mode
;; (setq js-indent-level preferred-javascript-indent-level)


;; ;; standard javascript-mode
;; (setq javascript-indent-level preferred-javascript-indent-level)

;; (add-to-list 'interpreter-mode-alist (cons "node" preferred-javascript-mode))


;; (eval-after-load 'coffee-mode
;;   `(setq coffee-js-mode preferred-javascript-mode
;;          coffee-tab-width preferred-javascript-indent-level))

;; (add-hook 'coffee-mode-hook 'flymake-coffee-load)
;; (add-to-list 'auto-mode-alist '("\\.coffee\\.erb\\'" . coffee-mode))

;; ;; ---------------------------------------------------------------------------
;; ;; Run and interact with an inferior JS via js-comint.el
;; ;; ---------------------------------------------------------------------------

;; (setq inferior-js-program-command "js")

;; (defvar inferior-js-minor-mode-map (make-sparse-keymap))
;; (define-key inferior-js-minor-mode-map "\C-x\C-e" 'js-send-last-sexp)
;; (define-key inferior-js-minor-mode-map "\C-\M-x" 'js-send-last-sexp-and-go)
;; (define-key inferior-js-minor-mode-map "\C-cb" 'js-send-buffer)
;; (define-key inferior-js-minor-mode-map "\C-c\C-b" 'js-send-buffer-and-go)
;; (define-key inferior-js-minor-mode-map "\C-cl" 'js-load-file-and-go)

;; (define-minor-mode inferior-js-keys-mode
;;   "Bindings for communicating with an inferior js interpreter."
;;   nil " InfJS" inferior-js-minor-mode-map)

;; (dolist (hook '(js2-mode-hook js3-mode-hook js-mode-hook))
;;   (add-hook hook 'inferior-js-keys-mode))

;; ;; ---------------------------------------------------------------------------
;; ;; Alternatively, use skewer-mode
;; ;; ---------------------------------------------------------------------------

;; (when (featurep 'js2-mode)
;;   (require-package 'skewer-mode)
;;   (add-hook 'skewer-mode-hook (lambda () (inferior-js-keys-mode -1))))


(provide 'init-javascript)
