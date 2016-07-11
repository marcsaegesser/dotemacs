;;-----------------------------------------------------
;;Initialization for Scala-mode-2
;;-----------------------------------------------------
(add-to-list 'load-path (expand-file-name "ensime-emacs" user-emacs-directory))

(require-package 'scala-mode)
(require-package 'ensime)

(use-package highlight-symbol
  :ensure t
  :diminish highlight-symbol-mode
  :commands highlight-symbol
  )

(require 'scala-mode)
(require 'ensime)

(defun scala-turn-off-indent-tabs-mode ()
  (setq indent-tabs-mode nil))

(add-hook 'scala-mode-hook 'scala-turn-off-indent-tabs-mode)

;;Scala-mode specific
(add-hook 'scala-mode-hook '(lambda ()
  (ensime-mode 1)

  ;; Ret takes care of proper indenting and asterisks in multiline comments
  (local-set-key (kbd "RET") '(lambda ()
    (interactive)
    (reindent-then-newline-and-indent)
    (scala-indent:insert-asterisk-on-multiline-comment)))

  (local-set-key (kbd "<backtab>") 'scala-indent:indent-with-reluctant-strategy)

  ;; Bind F5 to launch debugger
  (local-set-key (kbd "<f5>") 'ensime-db-run)

  (rainbow-delimiters-mode)
  (electric-pair-mode t)
  (setq prettify-symbols-alist scala-prettify-symbols-alist)
  (highlight-symbol-mode)
  (prettify-symbols-mode)
  (setq ensime-auto-generate-config t
        ensime-graphical-tooltips t
        ensime-implicit-gutter-icons nil
        ensime-sem-high-faces '((var . (:inherit font-lock-warning-face))
                                (val . (:inherit font-lock-constant-face :slant italic))
                                (varField . (:inherit font-lock-warning-face :weight bold))
                                (valField . (:inherit font-lock-constant-face :slant italic :weight bold))
                                (functionCall . font-lock-function-name-face)
                                (operator . font-lock-keyword-face)
                                (param . (:slant italic))
                                (class . font-lock-type-face)
                                (trait .  (:inherit font-lock-type-face :slant italic))
                                (object . font-lock-constant-face)
                                (package . font-lock-preprocessor-face)
                                (implicitConversion . ensime-implicit-highlight)
                                (implicitParams . ensime-implicit-highlight)
                                (deprecated . (:strike-through "dark gray")))
        ensime-server-version "2.0.0-SNAPSHOT"
        ensime-startup-snapshot-notification nil
        )
))

(provide 'init-scalamode2)
