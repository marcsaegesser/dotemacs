;;; The structure of this file is based heavily on John Wiegley's
;;; init.el (https://github.com/jwiegley/dot-emacs)
(require 'package)
(package-initialize)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
;; (add-to-list 'load-path user-emacs-directory)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Platform detection
(defconst *spell-check-support-enabled* nil)
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-carbon-emacs* (eq window-system 'mac))
(defconst *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns)))

;; Set up to use require-package.
;; Really only needed to ensure we can get use-package
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
(require 'init-elpa)      ;; Machinery for installing required packages

;; (eval-when-compile
;;   (require 'use-package))
(eval-and-compile
  (require-package 'use-package)

  (if init-file-debug
      (setq use-package-verbose t
            use-package-expand-minimally nil
            use-package-compute-statistics t
            debug-on-error t)
    (setq use-package-verbose nil
          use-package-expand-minimally t)))

;; Preferences
(fringe-mode 4)

(setq
 blink-cursor-delay 0
 browse-url-browser-function 'browse-url-generic
 browse-url-generic-program "google-chrome" 
 use-file-dialog nil
 use-dialog-box nil
 indicate-empty-lines t
 inhibit-startup-screen t
 inhibit-startup-echo-area-message t
 initial-scratch-message ""
 window-combintaion-resize t
)

(setq-default
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 compilation-scroll-output t
 delete-selection-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 grep-highlight-matches t
 grep-scroll-output t
 indent-tabs-mode nil
 make-backup-files nil
 mouse-yank-at-point t
 next-screen-context-lines 2
 save-interprogram-paste-before-kill t
 set-mark-command-repeat-pop t
 scroll-bar-mode 0
 scroll-margin 0
 scroll-conservatively 10
 show-trailing-whitespace t
 term-scroll-to-bottom-on-output 'this
 tooltip-delay 1.5
 truncate-lines t
 truncate-partial-width-windows nil
 visible-bell nil
 )

;; Change this. mas 2/10/2018
(cua-mode 0)
(cua-selection-mode t) ; for rectangles, CUA is nice

(setq x-select-enable-clipboard t)

(blink-cursor-mode 0)

(tool-bar-mode 0)
(menu-bar-mode -1)
(global-set-key [f9] 'toggle-menu-bar-mode-from-frame)

(when (fboundp 'electric-pair-mode)
  (setq-default electric-pair-mode 1))

(transient-mark-mode t)

(global-set-key (kbd "RET") 'newline-and-indent)

;; To be able to M-x without meta
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; More convenient join-line bindings
(global-set-key (kbd "C-M-S-j") 'join-line)
(global-set-key (kbd "C-M-j") (lambda () (interactive) (join-line 1)))

(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)

;; Buffer/Frame navigation
(global-set-key (kbd "C-S-n") 'next-buffer)
(global-set-key (kbd "C-S-p") 'previous-buffer)
(global-set-key (kbd "C-M-S-n") 'next-multiframe-window)
(global-set-key (kbd "C-M-S-p") 'previous-multiframe-window)

;; Move this someplace better? mas 2/10/2018
;; But don't show trailing whitespace in SQLi, inf-ruby etc.
(dolist (hook '(term-mode-hook comint-mode-hook compilation-mode-hook))
  (add-hook hook
            (lambda () (setq show-trailing-whitespace nil))))

;; Load packages

(use-package dash          :ensure t :defer)
(use-package diminish      :ensure t :demand t)
(use-package fringe-helper :ensure t :defer t)
(use-package s             :ensure t :defer)


;; (require 'init-auto-complete)  ;; this still needs work

(use-package avy
  :ensure t
  :init (setq avy-background t)
  :bind (("C-;" . avy-goto-subword-1)
         ("C-:" . avy-goto-word-0)
         ("M-g M-g" . avy-goto-line))
  )

(use-package avy-zap
  :ensure t
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

(use-package browse-kill-ring
  :ensure t
  :defer 5
  :commands browse-kill-ring)

(use-package color-theme-sanityinc-tomorrow  :ensure t)
(use-package color-theme-sanityinc-solarized :ensure t)

(use-package company
  :ensure t
  :defer 5
  :diminish
  :commands (company-mode company-indent-or-complete-common)
  :init
  (dolist (hook '(emacs-lisp-mode-hook
                  haskell-mode-hook
                  c-mode-common-hook))
    (add-hook hook
              #'(lambda ()
                  (local-set-key (kbd "<tab>")
                                 #'company-indent-or-complete-common))))
  :config
  ;; From https://github.com/company-mode/company-mode/issues/87
  ;; See also https://github.com/company-mode/company-mode/issues/123
  (defadvice company-pseudo-tooltip-unless-just-one-frontend
      (around only-show-tooltip-when-invoked activate)
    (when (company-explicit-action-p)
      ad-do-it))

  ;; See http://oremacs.com/2017/12/27/company-numbers/
  (defun ora-company-number ()
    "Forward to `company-complete-number'.
  Unless the number is potentially part of the candidate.
  In that case, insert the number."
    (interactive)
    (let* ((k (this-command-keys))
           (re (concat "^" company-prefix k)))
      (if (cl-find-if (lambda (s) (string-match re s))
                      company-candidates)
          (self-insert-command 1)
        (company-complete-number (string-to-number k)))))

  (let ((map company-active-map))
    (mapc
     (lambda (x)
       (define-key map (format "%d" x) 'ora-company-number))
     (number-sequence 0 9))
    (define-key map " " (lambda ()
                          (interactive)
                          (company-abort)
                          (self-insert-command 1))))

  (defun check-expansion ()
    (save-excursion
      (if (outline-on-heading-p t)
          nil
        (if (looking-at "\\_>") t
          (backward-char 1)
          (if (looking-at "\\.") t
            (backward-char 1)
            (if (looking-at "->") t nil))))))

  (define-key company-mode-map [tab]
    '(menu-item "maybe-company-expand" nil
                :filter (lambda (&optional _)
                          (when (check-expansion)
                            #'company-complete-common))))

  (eval-after-load "yasnippet"
    '(progn
       (defun company-mode/backend-with-yas (backend)
         (if (and (listp backend) (member 'company-yasnippet backend))
             backend
           (append (if (consp backend) backend (list backend))
                   '(:with company-yasnippet))))
       (setq company-backends
             (mapcar #'company-mode/backend-with-yas company-backends))))

  (global-company-mode 1))

(use-package company-elisp
  :after company
  :config
  (push 'company-elisp company-backends))

(setq-local company-backend '(company-elisp))

(use-package company-ghc
  :ensure t
  :after (company ghc)
  :config
  (push 'company-ghc company-backends))

(use-package crux
  :ensure t
  :bind (;;("C-c o" . crux-open-with)
         ;;("M-o" . crux-smart-open-line)
         ;; ("C-c n" . crux-cleanup-buffer-or-region)
         ;;("C-c f" . crux-recentf-ido-find-file)
         ;;("C-M-z" . crux-indent-defun)
         ;;("C-c u" . crux-view-url)
         ;;("C-c e" . crux-eval-and-replace)
         ;;("C-c w" . crux-swap-windows)
         ;;("C-c D" . crux-delete-file-and-buffer)
         ;;("C-c r" . crux-rename-buffer-and-file)
         ("C-c t" . crux-visit-term-buffer)
         ;;("C-c k" . crux-kill-other-buffers)
         ;;("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
         ;;("C-c I" . crux-find-user-init-file)
         ;;("C-c S" . crux-find-shell-init-file)
         ;;("s-r" . crux-recentf-ido-find-file)
         ;;("s-j" . crux-top-join-line)
         ;;("C-^" . crux-top-join-line)
         ;;("s-k" . crux-kill-whole-line)
         ;;("C-<backspace>" . crux-kill-line-backwards)
         ;;("s-o" . crux-smart-open-line-above)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([(shift return)] . crux-smart-open-line)
         ;;([(control shift return)] . crux-smart-open-line-above)
         ;;([remap kill-whole-line] . crux-kill-whole-line)
         ;;("C-c s" . crux-ispell-word-then-abbrev)
         ))

(use-package css-mode
  :ensure t
  :mode "\\.css\\'")

(use-package cursor-chg
  :ensure t
  :commands change-cursor-mode
  :config
  (change-cursor-mode 1)
  (toggle-cursor-type-when-idle 1))

(use-package default-text-scale
  :ensure t
  :diminish
  :config
  (default-text-scale-mode t))

(use-package dired-sidebar
  :ensure t
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (cond
   ((eq system-type 'darwin)
    (if (display-graphic-p)
        (setq dired-sidebar-theme 'icons)
      (setq dired-sidebar-theme 'nerd))
    (setq dired-sidebar-face '(:family "Helvetica" :height 140)))
   ((eq system-type 'windows-nt)
    (setq dired-sidebar-theme 'nerd)
    (setq dired-sidebar-face '(:family "Lucida Sans Unicode" :height 110)))
   (:default
    (setq dired-sidebar-theme 'nerd)
    (setq dired-sidebar-face '(:family "Iosevka Type" :height 100))))

  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t)

  (use-package all-the-icons-dired
    ;; M-x all-the-icons-install-fonts
    :ensure t
    :commands (all-the-icons-dired-mode)))

(use-package ensime
  :ensure t ;; This will use the non-stable version! See http://ensime.github.io/editors/emacs/install/
  :pin melpa-stable
  :preface
  (defun scala-ret-handler ()
    (interactive)
    (reindent-then-newline-and-indent)
    (scala-indent:insert-asterisk-on-multiline-comment))
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
        ;; ensime-startup-snapshot-notification nil ;; Acknowledge that we're crazy enough to use the dev branch.
        )
  )

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package flx-ido
  :ensure t
  :after ido
  :config
  (flx-ido-mode t))

(use-package flycheck-haskell
  :ensure t
  :after haskell-mode
  :config
  (flycheck-haskell-setup))

(use-package flymake-php :ensure t)

(use-package flymake-shell
  :ensure t
  :hook (sh-set-shell-hook . flymake-shell-load)
  )

(use-package flyspell
  :bind (("C-c i b" . flyspell-buffer)
         ("C-c i f" . flyspell-mode))
  :config
  (defun my-flyspell-maybe-correct-transposition (beg end candidates)
    (unless (let (case-fold-search)
              (string-match "\\`[A-Z0-9]+\\'"
                            (buffer-substring-no-properties beg end)))
      (flyspell-maybe-correct-transposition beg end candidates))))

(use-package git-gutter
  :ensure t
  :diminish
  :custom
  (git-gutter:update-interval 2))

(use-package git-gutter-fringe
  :ensure t
  :config
  (global-git-gutter-mode t))

(use-package haskell-mode
  :ensure t
  :mode (("\\.hs\\(c\\|-boot\\)?\\'" . haskell-mode)
         ("\\.lhs\\'" . literate-haskell-mode)
         ("\\.cabal\\'" . haskell-cabal-mode))
  :bind (:map haskell-mode-map
              ("C-c C-h" . my-haskell-hoogle)
              ("C-c C-," . haskell-navigate-imports)
              ("C-c C-." . haskell-mode-format-imports)
              ("C-c C-u" . my-haskell-insert-undefined)
              ("M-s")
              ("M-t"))
  :preface
  (defun my-haskell-insert-undefined ()
    (interactive) (insert "undefined"))

  (defun snippet (name)
    (interactive "sName: ")
    (find-file (expand-file-name (concat name ".hs") "~/src/notes"))
    (haskell-mode)
    (goto-char (point-min))
    (when (eobp)
      (insert "hdr")
      (yas-expand)))

  (defvar hoogle-server-process nil)
  (defun my-haskell-hoogle (query &optional arg)
    "Do a Hoogle search for QUERY."
    (interactive
     (let ((def (haskell-ident-at-point)))
       (if (and def (symbolp def)) (setq def (symbol-name def)))
       (list (read-string (if def
                              (format "Hoogle query (default %s): " def)
                            "Hoogle query: ")
                          nil nil def)
             current-prefix-arg)))
    (unless (and hoogle-server-process
                 (process-live-p hoogle-server-process))
      (message "Starting local Hoogle server on port 8687...")
      (with-current-buffer (get-buffer-create " *hoogle-web*")
        (cd temporary-file-directory)
        (setq hoogle-server-process
              (start-process "hoogle-web" (current-buffer) "hoogle"
                             "server" "--local" "--port=8687")))
      (message "Starting local Hoogle server on port 8687...done"))
    (browse-url
     (format "http://127.0.0.1:8687/?hoogle=%s"
             (replace-regexp-in-string
              " " "+" (replace-regexp-in-string "\\+" "%2B" query)))))

  (defvar haskell-prettify-symbols-alist
    '(("::"     . ?∷)
      ("forall" . ?∀)
      ("exists" . ?∃)
      ("->"     . ?→)
      ("<-"     . ?←)
      ("=>"     . ?⇒)
      ("~>"     . ?⇝)
      ("<~"     . ?⇜)
      ("<>"     . ?⨂)
      ("msum"   . ?⨁)
      ("\\"     . ?λ)
      ("not"    . ?¬)
      ("&&"     . ?∧)
      ("||"     . ?∨)
      ("/="     . ?≠)
      ("<="     . ?≤)
      (">="     . ?≥)
      ("<<<"    . ?⋘)
      (">>>"    . ?⋙)

      ("`elem`"             . ?∈)
      ("`notElem`"          . ?∉)
      ("`member`"           . ?∈)
      ("`notMember`"        . ?∉)
      ("`union`"            . ?∪)
      ("`intersection`"     . ?∩)
      ("`isSubsetOf`"       . ?⊆)
      ("`isProperSubsetOf`" . ?⊂)
      ("undefined"          . ?⊥)))

  :config
  (require 'haskell)
  (require 'haskell-doc)

  (defun my-haskell-mode-hook ()
    (haskell-indentation-mode)
    (interactive-haskell-mode)
    (diminish 'interactive-haskell-mode)
    (flycheck-mode 1)
    (setq-local prettify-symbols-alist haskell-prettify-symbols-alist)
    (prettify-symbols-mode 1)
    (bug-reference-prog-mode 1))

  (add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

  (eval-after-load 'align
    '(nconc
      align-rules-list
      (mapcar #'(lambda (x)
                  `(,(car x)
                    (regexp . ,(cdr x))
                    (modes quote (haskell-mode literate-haskell-mode))))
              '((haskell-types       . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                (haskell-assignment  . "\\(\\s-+\\)=\\s-+")
                (haskell-arrows      . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                (haskell-left-arrows . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+"))))))

(use-package highlight-symbol
  :ensure t
  :diminish highlight-symbol-mode
  :commands highlight-symbol
  )

(use-package hl-line
  :commands hl-line-mode
  :bind ("M-o h" . hl-line-mode))

;; (use-package hl-line+
;;   :after hl-line)

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :init
  (add-hook 'ibuffer-mode-hook
            #'(lambda ()
                (ibuffer-switch-to-saved-filter-groups "default")))
  :config
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                filename-and-process)))
  )

(use-package ibuffer-vc
  :ensure t
  :after ibuffer
  :commands ibuffer-vc-set-filter-groups-by-vc-root
  :hook (ibuffer . (lambda ()
                     (ibuffer-vc-set-filter-groups-by-vc-root)
                     (unless (eq ibuffer-sorting-mode 'alphabetic)
                       (ibuffer-do-sort-by-alphabetic)))))

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

(use-package ido-completing-read+
  :ensure t
  :config
  (ido-ubiquitous-mode t))

(use-package init-greek)

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

(use-package lua-mode
  :mode "\\.lua\\'"
  :interpreter "lua")

(use-package magit
  :ensure t
  :bind ("M-<f12>" . 'magit-status)
  :hook (magit-mode . hl-line-mode)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  )

(use-package markdown-mode
  :ensure t
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package markdown-preview-mode
  :ensure t
  :after markdown-mode
  :config
  (setq markdown-preview-stylesheets
        (list (concat "https://github.com/dmarcotte/github-markdown-preview/"
                      "blob/master/data/css/github.css"))))

(use-package memory-usage
  :ensure t
  :commands memory-usage)

(use-package mic-paren
  :ensure t
  :defer 5
  :config
  (paren-activate))

(use-package mmm-mode
  :ensure t
  :defer t
  :config
  (setq mmm-global-mode 'buffers-with-submode-classes)
  (setq mmm-submode-decoration-level 2))

(use-package multiple-cursors
  :ensure t
  :bind
  (("C-<" . mc/mark-previous-like-this)
   ("C->" . mc/mark-next-like-this)
   ("C-+" . mc/mark-next-like-this)
   ("C-c C-<" . mc/mark-all-like-this)
   ;; From active region to multiple cursors:
   ("C-c c r" . set-rectangular-region-anchor)
   ("C-c c c" . mc/edit-lines)
   ("C-c c e" . mc/edit-ends-of-lines)
   ("C-c c a" . mc/edit-beginnings-of-lines)))

(use-package multi-term
  :ensure t
  :bind (("C-c t" . multi-term-next)
         ("C-c T" . multi-term))
  :init
  (defun screen ()
    (interactive)
    (let (term-buffer)
      ;; Set buffer.
      (setq term-buffer
            (let ((multi-term-program (executable-find "screen"))
                  (multi-term-program-switches "-DR"))
              (multi-term-get-buffer)))
      (set-buffer term-buffer)
      (multi-term-internal)
      (switch-to-buffer term-buffer)))

  :config
  (require 'term)

  (defalias 'my-term-send-raw-at-prompt 'term-send-raw)

  (defun my-term-end-of-buffer ()
    (interactive)
    (call-interactively #'end-of-buffer)
    (if (and (eobp) (bolp))
        (delete-char -1)))

  (defadvice term-process-pager (after term-process-rebind-keys activate)
    (define-key term-pager-break-map  "\177" 'term-pager-back-page)))

(use-package nxml-mode
  :commands nxml-mode
  :bind (:map nxml-mode-map
              ("<return>" . newline-and-indent)
              ("C-c M-h"  . tidy-xml-buffer))
  :preface
  (defun tidy-xml-buffer ()
    (interactive)
    (save-excursion
      (call-process-region (point-min) (point-max) "tidy" t t nil
                           "-xml" "-i" "-wrap" "0" "-omit" "-q" "-utf8")))
  :init
  (defalias 'xml-mode 'nxml-mode)
  :config
  (autoload 'sgml-skip-tag-forward "sgml-mode")
  (add-to-list 'hs-special-modes-alist
               '(nxml-mode
                 "<!--\\|<[^/>]*[^/]>"
                 "-->\\|</[^/>]*[^/]>"
                 "<!--"
                 sgml-skip-tag-forward
                 nil)))

(use-package org
  :init
  (setq org-src-fontify-natively t))

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(use-package origami
  :ensure t
  :commands origami-mode
  :hook (prog-mode . origami-mode)
  :bind
  (("M-p"     . origami-recursively-toggle-node)
   ("C-c o t" . origami-toggle-node)
   ("C-c o o" . origami-open-node)
   ("C-c o O" . origami-open-node-recursively)
   ("C-c o c" . origami-close-node)
   ("C-c o C" . origami-close-node-recursively)
   ("C-c o t" . origami-toggle-node)
   ("C-c o T" . origami-forward-toggle-node)
   ("C-c o n" . origami-forward-fold)
   ("C-c o p" . origami-previous-fold)
   ("C-c o R" . origami-reset)))

(use-package page-break-lines
  :ensure t
  :diminish
  :config
  (global-page-break-lines-mode))

(use-package paredit
  :ensure t
  ;; :diminish
  :hook ((lisp-mode emacs-lisp-mode) . paredit-mode)
  :bind (:map paredit-mode-map
              ("[")
              ("M-k"       . paredit-raise-sexp)
              ("M-I"       . paredit-splice-sexp)
              ("C-M-l"     . paredit-recentre-on-sexp)
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
(use-package paredit-everywhere
  :ensure t
  :hook (prog-mode . paredit-everywhere-mode))

(use-package personal
  :defer t
  :bind (([remap open-line] . sanityinc/open-line-with-reindent))
)

(use-package php-mode
  :ensure t
  :hook (flymake-php-load)
  )

(use-package projectile
  :ensure t
  :diminish
  :config
  (setq projectile-globally-ignored-directories (append '(".ensime" ".ensime_cache" "target" "ami-server")))
  (projectile-global-mode))

(use-package pointback
  :ensure t
  :config
  (global-pointback-mode))

(use-package popup-imenu
  :ensure t
  :commands popup-imenu
  :bind ("M-i" . popup-imenu))

(use-package python-mode
  :mode "\\.py\\'"
  :interpreter "python"
  :bind (:map python-mode-map
              ("C-c c")
              ("C-c C-z" . python-shell))
  :config
  (defvar python-mode-initialized nil)

  (defun my-python-mode-hook ()
    (unless python-mode-initialized
      (setq python-mode-initialized t)

      (info-lookup-add-help
       :mode 'python-mode
       :regexp "[a-zA-Z_0-9.]+"
       :doc-spec
       '(("(python)Python Module Index" )
         ("(python)Index"
          (lambda
            (item)
            (cond
             ((string-match
               "\\([A-Za-z0-9_]+\\)() (in module \\([A-Za-z0-9_.]+\\))" item)
              (format "%s.%s" (match-string 2 item)
                      (match-string 1 item)))))))))

    (set (make-local-variable 'parens-require-spaces) nil)
    (setq indent-tabs-mode nil))

  (add-hook 'python-mode-hook 'my-python-mode-hook))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :commands rainbow-mode)

(use-package recentf
  :defer 10
  :commands (recentf-mode
             recentf-add-file
             recentf-apply-filename-handlers)
  :preface
  (defun recentf-add-dired-directory ()
    (if (and dired-directory
             (file-directory-p dired-directory)
             (not (string= "/" dired-directory)))
        (let ((last-idx (1- (length dired-directory))))
          (recentf-add-file
           (if (= ?/ (aref dired-directory last-idx))
               (substring dired-directory 0 last-idx)
             dired-directory)))))
  :hook (dired-mode . recentf-add-dired-directory)
  :config
  (recentf-mode 1))

(use-package regex-tool
  :load-path "lisp/regex-tool"
  :commands regex-tool)

(use-package sbt-mode
  :pin melpa)

(use-package scala-mode
  :ensure t
  :pin melpa
  :preface
  (defvar scala-prettify-symbols-alist
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
      ("<~" . ?⇜)))
  (defun my-scala-mode-hook ()
    (setq prettify-symbols-alist scala-prettify-symbols-alist
          indent-tabs-mode nil)
    (rainbow-delimiters-mode t)
    (electric-pair-mode t)
    ;; (smartparens-strict-mode t)
    (highlight-symbol-mode t)
    (prettify-symbols-mode t))
  :hook (scala-mode . my-scala-mode-hook))

(use-package selected
  :ensure t
  :defer 5
  :diminish selected-minor-mode
  :bind (:map selected-keymap
              ("[" . align-code)
              ("f" . fill-region)
              ("U" . unfill-region)
              ("d" . downcase-region)
              ("u" . upcase-region)
              ("r" . reverse-region)
              ("s" . sort-lines))
  :config
  (selected-global-mode 1))

(use-package server
  :unless noninteractive
  :no-require
  :hook (after-init . server-start))

(use-package smart-mode-line
  :ensure t
  :defer 10
  :config
  ;; See https://github.com/Malabarba/smart-mode-line/issues/217
  (setq mode-line-format (delq 'mode-line-position mode-line-format))
  (sml/setup)
  (sml/apply-theme 'respectful)
  (remove-hook 'display-time-hook 'sml/propertize-time-string))

(use-package smart-mode-line-powerline-theme
  :ensure t
  :disabled t
  :after smart-mode-line
  :config
  (sml/apply-theme 'powerline))

(use-package smex
  :ensure t
  :bind ("M-x" . 'smex))

(use-package smooth-scroll
  :ensure t
  :bind
  (("C-u"   . scroll-down)
   ("C-M-n" . scroll-up-1)
   ("C-M-p" . scroll-down-1)))

(use-package smooth-scrolling
  :ensure t
  :disabled
  :bind ("C-u" . scroll-down)
  :custom
  (smooth-scroll-margin 2)
  :config
  (smooth-scrolling-mode 1))

(use-package sql-indent
  :ensure t
  :commands sqlind-minor-mode)

(use-package switch-window
  :ensure t
  :config
  (setq switch-window-shortcut-style 'alphabet)
  :bind
  (("C-x o" . switch-window)
   ("C-x 9" . switch-window-then-delete)))

(use-package tidy
  :ensure t
  :commands (tidy-buffer
             tidy-parse-config-file
             tidy-save-settings
             tidy-describe-options))

(use-package undo-tree
  :ensure t
  :diminish
  :config
  (global-undo-tree-mode))

(use-package unfill
  :ensure t)

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator " • ")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package vline
  :ensure t
  :commands vline-mode)

(use-package volatile-highlights
  :ensure t
  :diminish
  :config
  (volatile-highlights-mode t))

(use-package wgrep
  :defer 5)

(use-package whole-line-or-region
  :ensure t
  :config
  (whole-line-or-region-mode t)
  (make-variable-buffer-local 'whole-line-or-region-mode))

(use-package workgroups2
  :ensure t
  :config
  (setq wg-emacs-exit-save-behavior 'save
        wg-flag-modified t)
  ;; (workgroups-mode 0)
  )

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

(use-package yasnippet
  :ensure t
  :after prog-mode
  :defer 10
  :diminish yas-minor-mode
  :bind (("C-c y d"   . yas-load-directory)
         ("C-c y i"   . yas-insert-snippet)
         ("C-c y f"   . yas-visit-snippet-file)
         ("C-c y n"   . yas-new-snippet)
         ("C-c y t"   . yas-tryout-snippet)
         ("C-c y l"   . yas-describe-tables)
         ("C-c y g"   . yas/global-mode)
         ("C-c y m"   . yas/minor-mode)
         ("C-c y a"   . yas-reload-all)
         ("C-c y TAB" . yas-expand)
         ("C-c y x"   . yas-expand))
  :bind (:map yas-keymap
              ("C-i" . yas-next-field-or-maybe-expand))
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :config
  (setq yas-snippet-dirs (append yas-snippet-dirs
                                 '("~/Downloads/interesting-snippets")))
  (yas-global-mode 1))

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t)
  )

(use-package zygospore
  :ensure t
  :bind ("C-x 1" . zygospore-toggle-delete-other-windows))


;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

