(use-package magit
  :ensure t)

(use-package git-timemachine
  :ensure t)

(use-package git-gutter
  :ensure t
  :config
  (progn (global-git-gutter-mode t)))

(provide 'etude-git)
