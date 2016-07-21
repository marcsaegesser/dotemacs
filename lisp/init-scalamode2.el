;;-----------------------------------------------------
;;Initialization for Scala-mode-2
;;-----------------------------------------------------
;; The following is used for ensime-emacs development work. Uncomment and then clone ensime-emacs into this directory.
(add-to-list 'load-path (expand-file-name "ensime-emacs" user-emacs-directory))

(use-package highlight-symbol
  :ensure t
  :diminish highlight-symbol-mode
  :commands highlight-symbol
  )

(defun scala-ret-handler ()
  (interactive)
  (reindent-then-newline-and-indent)
  (scala-indent:insert-asterisk-on-multiline-comment))

(defcustom
  scala-mode-prettify-symbols
  '(("<=" . ?≤)
    (">=" . ?≥)
    ("==" . ?≡)
    ("!" . ?¬)
    ("!=" . ?≢)
    ("&&" . ?∧)
    ("||" . ?∨)
    ("true" . ?⊤)
    ("false" . ?⊥)
    ("Int" . ?ℤ)
    ("Boolean" . ?𝔹)
    ("->" . ?→)
    ("<-" . ?←)
    ("=>" . ?⇒)
    ("<=>" . ?⇔)
    ("-->" . ?⟶)
    ("<->" . ?↔)
    ("<--" . ?⟵)
    ("<-->" . ?⟷)
    ("==>" . ?⟹)
    ("<==" . ?⟸)
    ("<==>" . ?⟺)
    ("~>" . ?⇝)
    ("<~" . ?⇜))
  "Prettify rules for arrow related code pieces.")

(use-package ensime
  ;; :ensure t ;; This will use the non-stable version! See http://ensime.github.io/editors/emacs/install/
  :bind (
         :map ensime-mode-map
              ("RET" . scala-ret-handler) ;; Note to self: Why can't I use a lambda here?
              ("<backtab>" . scala-indent:indent-with-reluctant-strategy)
              ("C-c C-v t" . ensime-type-at-point-full-name) ;; Swap standard type-at-point bindings so that
              ("C-c C-v T" . ensime-type-at-point))          ;; the easier one to type provides the full type
  :config
  (setq ensime-auto-generate-config t
        ensime-graphical-tooltips t
        ensime-implicit-gutter-icons nil
        ensime-sbt-perform-on-save "compile"
        ;; Modify default faces with bold for varField and valField
        ensime-sem-high-faces (nconc '((varField . (:inherit font-lock-warning-face :weight bold))
                                       (valField . (:inherit font-lock-constant-face :slant italic :weight bold)))
                                     ensime-sem-high-faces)
        ensime-server-version "2.0.0-SNAPSHOT"   ;; Track development branch of the server
        ensime-startup-snapshot-notification nil ;; Acknowledge that we're crazy enough to use the dev branch.
        )
  )

(add-hook 'scala-mode-hook
          (lambda ()
            (setq prettify-symbols-alist scala-mode-prettify-symbols
                  indent-tabs-mode nil)
            (rainbow-delimiters-mode t)
            (electric-pair-mode t)
            (highlight-symbol-mode t)
            (prettify-symbols-mode t)))

(provide 'init-scalamode2)
