(use-package etude-ivy)

(use-package projectile 
  :ensure t)

(setq projectile-completion-system 'ivy)

(use-package counsel-projectile
  :ensure t
  :config
  (add-hook 'after-init-hook 'counsel-projectile-mode))

(provide 'etude-projectile)
