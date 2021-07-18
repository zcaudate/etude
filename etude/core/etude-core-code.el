
;; Automatic completion
(define-key read-expression-map (kbd "TAB") 'completion-at-point)

(use-package company
  :ensure t
  :diminish 'company-mode
  :init   (progn (setq company-idle-delay 0.05)
                 (setq company-tooltip-limit 20)
                 (setq company-minimum-prefix-length 1)
                 (setq company-tooltip-flip-when-above t)
                 (setq company-show-numbers t))
  :config (progn (add-hook 'after-init-hook 'global-company-mode)
                 (define-key company-active-map
                   " "
                   (lambda ()
                     (interactive)
                     (company-abort)
                     (self-insert-command 1)))
                 (define-key company-active-map (kbd "<return>") nil)))

(use-package company-fuzzy :defer t)

(provide 'etude-core-code)
