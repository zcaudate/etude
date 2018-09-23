(use-package etude-neotree)
(use-package etude-projectile)

(use-package dashboard
  :ensure t)

(setq dashboard-startup-banner nil)

(setq dashboard-items '((recents  . 15)
			(projects . 5)))

(defun dashboard-blank ()
  (interactive)
  (dashboard-insert-startupify-lists)
  (switch-to-buffer "*dashboard*")
  (redisplay)
  (neotree-show)
  (switch-to-buffer "*dashboard*"))

(progn
  (add-hook 'emacs-startup-hook 'dashboard-blank))

(provide 'etude-dashboard)
