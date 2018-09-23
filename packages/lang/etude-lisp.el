(use-package paredit
  :ensure t
  :diminish paredit-mode)

(use-package rainbow-delimiters
  :ensure t)

;; Lisp configuration
(define-key read-expression-map (kbd "TAB") 'completion-at-point)

(provide 'etude-lisp)
