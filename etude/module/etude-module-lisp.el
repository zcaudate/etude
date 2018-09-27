(ns: etude-module-lisp
  (:require etude-core))

(defun on/jump-to-elisp-config ()
  (interactive)
  (find-library "etude-module-lisp"))

(progn (add-hook 'lisp-interaction-mode-hook 'smartparens-strict-mode)
       (add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
       (add-hook 'lisp-interaction-mode-hook 'paredit-mode)
       (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
       (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
       (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(on/mode: [::lisp   lisp-interaction-mode]
  "etude-module-lisp"
  ::eval-cursor    'eval-last-sexp
  ::eval-buffer    'eval-buffer)

(on/mode: [::emacs-lisp    emacs-lisp-mode]
  "etude-module-lisp" 
  ::eval-cursor    'eval-last-sexp
  ::eval-buffer    'eval-buffer)

(on/bind: [emacs-lisp-mode-map]
  ::jump-config  ("M-)")   'on/jump-to-elisp-config
  ::jump-back    ("M-0")   'on/jump-back)

(use-package geiser
  :defer true
  :config (progn (setq geiser-active-implementations '(racket))
                 (setq geiser-mode-autodoc-p nil)
                 (on/mode: [::scheme    geiser-mode]
                   "etude-module-lisp" 
                   ::eval-cursor    'geiser-eval-last-sexp
                   ::eval-buffer    'geiser-eval-buffer)))
