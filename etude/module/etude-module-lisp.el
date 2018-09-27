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

(defun on/eval-buffer ()
  (interactive)
  (eval-buffer (current-buffer) true))

(on/mode: [::lisp   lisp-interaction-mode]
  ::eval-cursor   'eval-last-sexp
  ::eval-file     'on/eval-buffer)

(on/mode: [::emacs-lisp    emacs-lisp-mode]
  ::eval-cursor  'eval-last-sexp
  ::eval-file    'on/eval-buffer)

(on/bind: [emacs-lisp-mode-map]
  ::jump-config  ("M-)")   'on/jump-to-elisp-config
  ::jump-back    ("M-0")   'on/jump-back)

(use-package geiser
  :defer true
  :config (progn (setq geiser-active-implementations '(racket))
                 (setq geiser-mode-autodoc-p nil)
                 (on/mode: [::scheme geiser-mode]
                   ::eval-cursor    'geiser-eval-last-sexp
                   ::eval-file      'geiser-eval-buffer)))
