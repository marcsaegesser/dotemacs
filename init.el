;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

;; (add-to-list 'load-path user-emacs-directory)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
;; (require 'init-benchmarking) ;; Measure startup time

;;----------------------------------------------------------------------------
;; Which functionality to enable (use t or nil for true and false)
;;----------------------------------------------------------------------------
(defconst *spell-check-support-enabled* nil)
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-carbon-emacs* (eq window-system 'mac))
(defconst *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns)))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
;; (require 'init-compat)
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(require-package 'use-package)

(eval-when-compile
  (require 'use-package))

(use-package diminish :demand t)
;; (require 'bind-key)                ;; if you use any :bind variant

(use-package wgrep :defer 5)
;; (require-package 'wgrep)
;; (require-package 'project-local-variables)
;; (require-package 'diminish)
;; (require-package 'scratch)
;; (require-package 'mwe-log-commands)

;; (require 'init-frame-hooks)
;; (require 'init-xterm)
;; (require 'init-themes)
;; (require 'init-osx-keys)
(require 'init-gui-frames)
;; (require 'init-maxframe)
;; (require 'init-proxies)
(require 'init-dired)
;; (require 'init-isearch)
(require 'init-uniquify)
(require 'init-ibuffer)
;; (require 'init-flymake)

(require 'init-recentf)
(require 'init-ido)
;; (require 'init-hippie-expand)
(require 'init-auto-complete)
(require 'init-windows)
;; (require 'init-sessions)
(require 'init-fonts)
(require 'init-mmm)
;; (require 'init-growl)

(require 'init-editing-utils)
(require 'init-greek)

;; (require 'init-darcs)
(require 'init-git)

;; (require 'init-crontab)
;; (require 'init-textile)
(require 'init-markdown)
;; (require 'init-csv)
;; (require 'init-erlang)
(require 'init-javascript)
(require 'init-sh)
(require 'init-php)
(require 'init-org)
(require 'init-nxml)
(require 'init-css)
;; (require 'init-haml)
(require 'init-python-mode)
(require 'init-haskell)
;; (require 'init-ruby-mode)
;; (require 'init-rails)
(require 'init-sql)
(require 'init-scalamode2)
;; (require 'init-ecb)
;; (require 'init-ocaml)

(require 'init-paredit)
(require 'init-lisp)
;; (require 'init-slime)
;; (require 'init-clojure)
;; (require 'init-common-lisp)

;; (require 'init-erc)
(require 'init-ansi-term)

;;(require 'init-guidekey)

(when *spell-check-support-enabled*
  (require 'init-spelling))

(require 'init-marmalade)
;; (require 'init-misc)
(require 'init-workgroups)

(require 'init-crux)

;; (require 'init-xiki)

;; Extra packages which don't require any configuration

(require-package 'projectile)
(setq projectile-globally-ignored-directories (append '(".ensime" ".ensime_cache" "target" "ami-server")))
(projectile-mode)

(require-package 'gnuplot)
(require-package 'lua-mode)
(require-package 'htmlize)
(require-package 'dsvn)
(when *is-a-mac*
  (require-package 'osx-location))
(require-package 'regex-tool)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

(fringe-mode 4)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))


;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-local" containing personal settings
;;----------------------------------------------------------------------------
(require 'init-local nil t)


;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(require 'init-locales)

;;----------------------------------------------------------------------------
;; Make all the custom themes available
;;----------------------------------------------------------------------------
(require 'dash)
(require 's)

(-each
   (-map
      (lambda (item)
      (format "~/.emacs.d/elpa/%s" item))
   (-filter
      (lambda (item) (s-contains? "theme" item))
      (directory-files "~/.emacs.d/elpa/")))
   (lambda (item)
      (add-to-list 'custom-theme-load-path item)))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
