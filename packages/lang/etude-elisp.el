(use-package etude-lisp
  :config (progn
	    (add-hook 'lisp-interaction-mode-hook 'smartparens-strict-mode)
            (add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
            (add-hook 'lisp-interaction-mode-hook 'paredit-mode)

	    (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
            (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
            (add-hook 'emacs-lisp-mode-hook 'paredit-mode))
  :bind   (("C-c C-k" . eval-buffer)))

(provide 'etude-elisp)
