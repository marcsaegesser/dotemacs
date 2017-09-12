(require-package 'magit)
(require-package 'git-gutter-fringe)
;; (require-package 'git-blame)
;; (require-package 'git-commit-mode)
(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)

(setq-default
 magit-save-some-buffers nil
 magit-process-popup-time 10
 magit-diff-refine-hunk t
 magit-completing-read-function 'magit-ido-completing-read)

(global-set-key [(meta f12)] 'magit-status)

(eval-after-load 'magit
  '(progn
     ;; Don't let magit-status mess up window configurations
     ;; http://whattheemacsd.com/setup-magit.el-01.html
     (defadvice magit-status (around magit-fullscreen activate)
       (window-configuration-to-register :magit-fullscreen)
       ad-do-it
       (delete-other-windows))

     (defun magit-quit-session ()
       "Restores the previous window configuration and kills the magit buffer"
       (interactive)
       (kill-buffer)
       (when (get-register :magit-fullscreen)
         (ignore-errors
           (jump-to-register :magit-fullscreen))))

     (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)))


;;; When we start working on git-backed files, use git-wip if available

;; (eval-after-load 'vc-git
;;   '(progn
;;      (global-magit-wip-save-mode)
;;      (diminish 'magit-wip-save-mode)))



(use-package git-gutter
  :ensure t
  :config
  (custom-set-variables
   '(git-gutter:update-interval 2))
  :diminish git-gutter-mode
  :commands git-gutter-mode)


;;; Use the fringe version of git-gutter

(eval-after-load 'git-gutter
  '(require 'git-gutter-fringe))


(when *is-a-mac*
  (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)]))))



;; Convenient binding for vc-git-grep
(global-set-key (kbd "C-x v f") 'vc-git-grep)



;;; git-svn support

(eval-after-load 'magit-key-mode
  '(progn
     (require 'magit-svn)))

(eval-after-load 'compile
  '(progn
     (dolist (defn (list '(git-svn-updated "^\t[A-Z]\t\\(.*\\)$" 1 nil nil 0 1)
                         '(git-svn-needs-update "^\\(.*\\): needs update$" 1 nil nil 2 1)))
       (add-to-list 'compilation-error-regexp-alist-alist defn)
       (add-to-list 'compilation-error-regexp-alist (car defn)))))

(defvar git-svn--available-commands nil "Cached list of git svn subcommands")

(defun git-svn (dir)
  "Run a git svn subcommand in DIR."
  (interactive "DSelect directory: ")
  (unless git-svn--available-commands
    (setq git-svn--available-commands
          (string-all-matches "^  \\([a-z\\-]+\\) +" (shell-command-to-string "git svn help") 1)))
  (let* ((default-directory (vc-git-root dir))
         (compilation-buffer-name-function (lambda (major-mode-name) "*git-svn*")))
    (compile (concat "git svn "
                     (ido-completing-read "git-svn command: " git-svn--available-commands nil t)))))



;;; github

(require-package 'yagist)
(require-package 'github-browse-file)
(require-package 'bug-reference-github)
(add-hook 'prog-mode 'bug-reference-prog-mode)



(provide 'init-git)
