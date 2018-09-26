(ns: etude-module-emacs-lisp
  (:require etude-core))

(defun on/jump-to-elisp-config ()
  (interactive)
  (find-library "etude-lang-emacs-lisp"))

(progn (add-hook 'lisp-interaction-mode-hook 'smartparens-strict-mode)
       (add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
       (add-hook 'lisp-interaction-mode-hook 'paredit-mode)
       (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
       (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
       (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(on/mode: [::lisp   lisp-interaction-mode]
  "etude-module-emacs-lisp" 
  ::eval-cursor    'eval-last-sexp
  ::eval-buffer    'eval-buffer)

(on/mode: [::emacs-lisp    emacs-lisp-mode]
  "etude-module-emacs-lisp" 
  ::eval-cursor    'eval-last-sexp
  ::eval-buffer    'eval-buffer)

(on/bind: [emacs-lisp-mode-map]
  ::jump-config  ("M-)")   'on/jump-to-elisp-config
  ::jump-back    ("M-0")   'on/jump-back)
