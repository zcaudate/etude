(use-package company
  :ensure t
  :diminish company-mode
  :config (add-hook 'after-init-hook 'global-company-mode)
  :init (progn 
          (setq company-idle-delay 0.05)
          (setq company-tooltip-limit 20)
          (setq company-minimum-prefix-length 1)
          (setq company-tooltip-flip-when-above t)))

(provide 'etude-company)