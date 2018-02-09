(use-package workgroups2
  :ensure t
  :config
  (setq wg-emacs-exit-save-behavior           'save
        wg-workgroups-mode-exit-save-behavior 'save
        wg-flag-modified t)
  (workgroups-mode t))



(provide 'init-workgroups)
