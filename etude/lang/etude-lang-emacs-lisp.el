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


(mode:
 (::emacs-lisp    emacs-lisp-mode
		  "etude-lang-emacs-lisp") 
 ::eval-cursor    'eval-last-sexp
 ::eval-buffer    'eval-buffer)

(bind: emacs-lisp-mode-map
 ::jump-config  ("M-)")   'etude/jump-to-elisp-config
 ::jump-back    ("M-0")   'etude/jump-back)

(provide 'etude-lang-emacs-lisp)
