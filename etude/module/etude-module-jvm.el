(ns: etude-module-jvm
  (:require etude-core))

;;
;; Java
;;

(use-package jdee
  :defer t)

;;
;; Clojure
;;

(use-package clojure-mode
  :defer t
  :init (progn (add-hook 'clojure-mode-hook 'smartparens-strict-mode)
               (add-hook 'clojure-mode-hook 'paredit-mode)
               (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)))

(use-package midje-mode
  :defer t
  :config (define-clojure-indent
             (comment 'defun)))

(use-package cider
  :defer t
  :init (progn (add-hook 'cider-mode-hook 'eldoc-mode)
               (setq nrepl-log-messages t)
               (setq cider-prefer-local-resources t)
               (setq nrepl-buffer-name-separator "/")
               (setq nrepl-buffer-name-show-port t)
               (setq cider-repl-use-clojure-font-lock t)
               (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
               (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
               (add-hook 'cider-repl-mode-hook 'subword-mode)
               (setq cider-cljs-lein-repl
                     "(do (require 'figwheel-sidecar.repl-api)
                          (figwheel-sidecar.repl-api/start-figwheel!)
                          (figwheel-sidecar.repl-api/cljs-repl))")))

(on/mode: [::clojure clojure-mode]
  "etude-module-clojure"
  ::eval-cursor   'cider-eval-last-sexp
  ::eval-file     'cider-eval-buffer
  ::init          'cider-connect)

           

