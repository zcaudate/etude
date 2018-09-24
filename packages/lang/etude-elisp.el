(defun open-elisp-config ()
  (interactive)
  (find-file "~/.emacs.d/packages/lang/etude-elisp.el"))

(defun open-previous-buffer ()
  (interactive)
  (previous-buffer))

(use-package etude-lisp
  :config (progn
	    (add-hook 'lisp-interaction-mode-hook 'smartparens-strict-mode)
            (add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
            (add-hook 'lisp-interaction-mode-hook 'paredit-mode)

	    (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
            (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
            (add-hook 'emacs-lisp-mode-hook 'paredit-mode))
  :bind   (:map emacs-lisp-mode-map
	   ("C-c C-k" . eval-buffer)
	   ("C-e" . eval-last-sexp)
	   ("M-9" . open-elisp-config)
	   ("M-0" . open-previous-buffer)))

(provide 'etude-elisp)
