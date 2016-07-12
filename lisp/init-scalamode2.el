;;-----------------------------------------------------
;;Initialization for Scala-mode-2
;;-----------------------------------------------------
;; The following is used for ensime-emacs development work. Uncomment and then clone ensime-emacs into this directory.
;; (add-to-list 'load-path (expand-file-name "ensime-emacs" user-emacs-directory))

(use-package highlight-symbol
  :ensure t
  :diminish highlight-symbol-mode
  :commands highlight-symbol
  )

(defun scala-ret-handler ()
  (interactive)
  (reindent-then-newline-and-indent)
  (scala-indent:insert-asterisk-on-multiline-comment))

(use-package ensime
  :ensure t ;; This will use the non-stable version! See http://ensime.github.io/editors/emacs/install/
  :bind (("RET" . scala-ret-handler) ;; Note to self: Why can't I use a lambda here?
         ("<backtab>" . scala-indent:indent-with-reluctant-strategy)
         ("<f5>" . ensime-db-run))
  :config
  (rainbow-delimiters-mode)
  (electric-pair-mode t)
  (highlight-symbol-mode)
  (setq prettify-symbols-alist scala-prettify-symbols-alist)
  (prettify-symbols-mode)
  (setq ensime-auto-generate-config t
        indent-tabs-mode nil
        ensime-graphical-tooltips t
        ensime-implicit-gutter-icons nil
        ;; Modify default faces with bold for varField and valField
        ensime-sem-high-faces (nconc '((varField . (:inherit font-lock-warning-face :weight bold))
                                       (valField . (:inherit font-lock-constant-face :slant italic :weight bold)))
                                     ensime-sem-high-faces)
        ensime-server-version "2.0.0-SNAPSHOT"   ;; Track development branch of the server
        ensime-startup-snapshot-notification nil ;; Acknowledge that we're crazy enough to use the dev branch.
        )
  )

(provide 'init-scalamode2)
