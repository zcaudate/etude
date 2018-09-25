(require 'etude-global)

(defun etude/jump-to-elisp-config ()
  (interactive)
  (find-library "etude-lang-emacs-lisp"))

(use-package etude-global-actions
  :init   (progn (add-hook 'lisp-interaction-mode-hook 'smartparens-strict-mode)
		 (add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
		 (add-hook 'lisp-interaction-mode-hook 'paredit-mode)
		 (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
		 (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
		 (add-hook 'emacs-lisp-mode-hook 'paredit-mode))
  :bind   (:map
	   emacs-lisp-mode-map
	   ("C-c C-k" . eval-buffer)
	   ("M-e" . eval-buffer)
      	   ("C-e" . eval-last-sexp)
      	   ("M-)" . etude/jump-to-elisp-config)
      	   ("M-0" . etude/jump-back)

	   :map
	   lisp-interaction-mode-map
	   ("C-c C-k" . eval-buffer)
	   ("M-e" . eval-buffer)
      	   ("C-e" . eval-last-sexp)
      	   ("M-)" . etude/jump-to-elisp-config)
      	   ("M-0" . etude/jump-back)))

(comment:
 
 (bind:
  (::emacs-lisp :config "etude-lang-emacs-lisp"
		:hook emacs-lisp-mode-hook
		:keys emacs-lisp-mode-map)
  
  :eval          ()        'eval-last-sexp
  :eval-buffer   ()        'eval-buffer
  :jump-config   ("M-)")   'etude/jump-to-elisp-config))


(provide 'etude-lang-emacs-lisp)
