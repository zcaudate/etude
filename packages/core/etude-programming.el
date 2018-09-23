(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config (progn
	    (require 'smartparens-config)
	    (smartparens-global-mode 1)
	    (show-paren-mode 1)))

(provide 'etude-programming)
