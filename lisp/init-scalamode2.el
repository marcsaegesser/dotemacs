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

(use-package popup-imenu
  :ensure t
  :commands popup-imenu
  :bind ("M-i" . popup-imenu))

;; Borrowed from Sam Halliday but I haven't been able to make this transistion work for me, yet
;; (use-package smartparens
;;   :ensure t
;;   :diminish smartparens-mode
;;   :commands
;;   smartparens-strict-mode
;;   smartparens-mode
;;   sp-restrict-to-pairs-interactive
;;   sp-local-pair
;;   :init
;;   (setq sp-interactive-dwim t)
;;   :config
;;   (require 'smartparens-config)
;;   (sp-use-smartparens-bindings)
;;   (sp-pair "(" ")" :wrap "C-(") ;; how do people live without this?
;;   (sp-pair "[" "]" :wrap "s-[") ;; C-[ sends ESC
;;   (sp-pair "{" "}" :wrap "C-{")
;;   (sp-pair "<" ">" :wrap "C-<")

;;   ;; nice whitespace / indentation when creating statements
;;   (sp-local-pair '(c-mode java-mode) "(" nil :post-handlers '(("||\n[i]" "RET")))
;;   (sp-local-pair '(c-mode java-mode) "{" nil :post-handlers '(("||\n[i]" "RET")))
;;   (sp-local-pair '(java-mode) "<" nil :post-handlers '(("||\n[i]" "RET")))

;;   ;; WORKAROUND https://github.com/Fuco1/smartparens/issues/543
;;   (bind-key "C-<left>" nil smartparens-mode-map)
;;   (bind-key "C-<right>" nil smartparens-mode-map)

;;   (bind-key "s-{" 'sp-rewrap-sexp smartparens-mode-map)

;;   (bind-key "s-<delete>" 'sp-kill-sexp smartparens-mode-map)
;;   (bind-key "s-<backspace>" 'sp-backward-kill-sexp smartparens-mode-map)
;;   (bind-key "s-<home>" 'sp-beginning-of-sexp smartparens-mode-map)
;;   (bind-key "s-<end>" 'sp-end-of-sexp smartparens-mode-map)
;;   (bind-key "s-<left>" 'sp-beginning-of-previous-sexp smartparens-mode-map)
;;   (bind-key "s-<right>" 'sp-next-sexp smartparens-mode-map)
;;   (bind-key "s-<up>" 'sp-backward-up-sexp smartparens-mode-map)
;;   (bind-key "s-<down>" 'sp-down-sexp smartparens-mode-map))

(use-package ensime
  :ensure t ;; This will use the non-stable version! See http://ensime.github.io/editors/emacs/install/
  :pin melpa
  :bind (
         :map ensime-mode-map
              ("RET" . scala-ret-handler) ;; Note to self: Why can't I use a lambda here?
              ("<backtab>" . scala-indent:indent-with-reluctant-strategy)
              ("C-c C-v t" . ensime-type-at-point-full-name) ;; Swap standard type-at-point bindings so that
              ("C-c C-v T" . ensime-type-at-point))          ;; the easier one to type provides the full type
  :config
  (setq ensime-auto-generate-config t
        ensime-graphical-tooltips nil
        ensime-implicit-gutter-icons nil
        ensime-startup-notification nil
        ;; ensime-sbt-perform-on-save "compile"
        ;; Modify default faces with bold for varField and valField
        ensime-sem-high-faces (nconc '((varField . (:inherit font-lock-warning-face :weight bold))
                                       (valField . (:inherit font-lock-constant-face :slant italic :weight bold)))
                                     ensime-sem-high-faces)
        ;; ensime-server-version "2.0.0-graph-SNAPSHOT"   ;; Track development branch of the server
        ensime-startup-snapshot-notification nil ;; Acknowledge that we're crazy enough to use the dev branch.
        )
  )

(use-package sbt-mode
  :pin melpa)

(use-package scala-mode
  :pin melpa)

(add-hook 'scala-mode-hook
          (lambda ()
            (setq prettify-symbols-alist scala-mode-prettify-symbols
                  indent-tabs-mode nil)
            (rainbow-delimiters-mode t)
            (electric-pair-mode t)
            ;; (smartparens-strict-mode t)
            (highlight-symbol-mode t)
            (prettify-symbols-mode t)
            (git-gutter-mode t)))

(provide 'init-scalamode2)
