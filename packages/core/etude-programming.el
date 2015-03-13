(use-package projectile 
  :ensure t
  :config
  (progn
    (global-set-key (kbd "s-t") 'projectile-find-file)
    (global-set-key (kbd "C-c f") 'projectile-find-file)))
    
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (show-paren-mode 1)))
    
(provide 'etude-programming)