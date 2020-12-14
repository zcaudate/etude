(ns: etude-module-lisp
  (:require etude-core))

(defun on/jump-to-elisp-config ()
  (interactive)
  (find-library "etude-module-lisp"))

(progn (add-hook 'lisp-interaction-mode-hook 'smartparens-strict-mode)
       (add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
       (add-hook 'lisp-interaction-mode-hook 'paredit-mode)
       (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
       (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
       (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
       (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
       (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

(defun on/eval-buffer ()
  (interactive)
  (eval-buffer (current-buffer) t))

(on/mode: [::lisp   lisp-interaction-mode "etude-module-lisp"]
  ::eval-cursor   'eval-last-sexp
  ::eval-file     'on/eval-buffer)

(on/mode: [::emacs-lisp    emacs-lisp-mode "etude-module-lisp"]
  ::eval-cursor  'eval-last-sexp
  ::eval-file    'on/eval-buffer)

(on/bind: [emacs-lisp-mode-map]
  ::jump-config  ("M-)")   'on/jump-to-elisp-config
  ::jump-back    ("M-0")   'on/jump-back)

(on/mode: [::eshell-mode eshell-mode    "etude-module-lisp"]
  ::eval-cursor 'eval-last-sexp)

;;
;; Clojure
;;


(use-package cider
  :ensure t
  :init (progn (setq nrepl-log-messages t)
               (setq nrepl-buffer-name-separator "/")
               (setq nrepl-buffer-name-show-port t)
               (setq cider-prefer-local-resources t)
               (setq cider-repl-use-clojure-font-lock t)
               (setq cider-cljs-lein-repl
                     "(do (require 'figwheel-sidecar.repl-api)
                          (figwheel-sidecar.repl-api/start-figwheel!)
                          (figwheel-sidecar.repl-api/cljs-repl))"))
  :hook ((cider-repl-mode . smartparens-strict-mode)
         (cider-repl-mode . rainbow-delimiters-mode)
         (cider-repl-mode . eldoc-mode)))

(defun on/cider-eval-buffer ()
  (interactive)
  (save-buffer)
  (cider-eval-buffer))

(use-package clojure-mode
  :ensure t
  :config (progn (require 'cider-mode)
                 (require 'midje-mode)
                 (on/mode: [::clojure clojure-mode "etude-module-jvm"]
                   ::eval-cursor   'cider-eval-last-sexp
                   ::eval-file     'on/cider-eval-buffer
                   ::init          'cider-connect))
  :hook ((clojure-mode . smartparens-strict-mode)
         (clojure-mode . rainbow-delimiters-mode)
         (clojure-mode . paredit-mode)
         (clojure-mode . eldoc-mode)))

(use-package midje-mode
  :ensure t
  :config (define-clojure-indent
            (comment 'defun)))

;;
;; Overlays from cider
;;

(autoload 'cider--make-result-overlay "cider-overlays")

(defun endless/eval-overlay (value point)
  (cider--make-result-overlay (format "%S" value)
    :where point
    :duration 'command)
  ;; Preserve the return value.
  value)

(advice-add 'eval-region :around
            (lambda (f beg end &rest r)
              (endless/eval-overlay
               (apply f beg end r)
               end)))

(advice-add 'eval-last-sexp :filter-return
            (lambda (r)
              (endless/eval-overlay r (point))))

(advice-add 'eval-defun :filter-return
            (lambda (r)
              (endless/eval-overlay
               r
               (save-excursion
                 (end-of-defun)
                 (point)))))
