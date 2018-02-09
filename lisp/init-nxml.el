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

(use-package tidy
  :ensure t
  :commands (tidy-buffer
             tidy-parse-config-file
             tidy-save-settings
             tidy-describe-options))

;; (add-auto-mode
;;  'nxml-mode
;;  (concat "\\."
;;          (regexp-opt
;;           '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss"
;;             "gpx" "tcx"))
;;          "\\'"))
;; (setq magic-mode-alist (cons '("<\\?xml " . nxml-mode) magic-mode-alist))
;; (fset 'xml-mode 'nxml-mode)
;; (add-hook 'nxml-mode-hook (lambda ()
;;                             (set (make-local-variable 'ido-use-filename-at-point) nil)))
;; (setq nxml-slash-auto-complete-flag t)


;; See: http://sinewalker.wordpress.com/2008/06/26/pretty-printing-xml-with-emacs-nxml-mode/
;; (defun pp-xml-region (begin end)
;;   "Pretty format XML markup in region. The function inserts
;; linebreaks to separate tags that have nothing but whitespace
;; between them.  It then indents the markup by using nxml's
;; indentation rules."
;;   (interactive "r")
;;   (save-excursion
;;       (nxml-mode)
;;       (goto-char begin)
;;       (while (search-forward-regexp "\>[ \\t]*\<" nil t)
;;         (backward-char) (insert "\n"))
;;       (indent-region begin end)))

;;----------------------------------------------------------------------------
;; Integration with tidy for html + xml
;;----------------------------------------------------------------------------
;; (require-package 'tidy)
;; (add-hook 'nxml-mode-hook (lambda () (tidy-build-menu nxml-mode-map)))
;; (add-hook 'html-mode-hook (lambda () (tidy-build-menu html-mode-map)))


;; (add-auto-mode 'html-mode "\\.(jsp|tmpl)\\'")


(provide 'init-nxml)
