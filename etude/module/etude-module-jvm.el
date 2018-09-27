(ns: etude-module-jvm
  (:require etude-core))

;;
;; Java
;;
(setenv "JAVA_HOME" "/Library/Java/JavaVirtualMachines/jdk-10.0.2.jdk")

(require 'cedet)

(use-package jdee
  :defer true
  :config (setq jdee-server-dir (s-concat emacs-d "dev")))

;;
;; Clojure
;;

(use-package clojure-mode
  :defer true
  :init   (progn (add-hook 'clojure-mode-hook 'smartparens-strict-mode)
                 (add-hook 'clojure-mode-hook 'paredit-mode)
                 (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode))
  :config (progn (require 'cider-mode)
                 (require 'midje-mode)
                 (on/mode: [::clojure clojure-mode]
                   ::eval-cursor   'cider-eval-last-sexp
                   ::eval-file     'cider-eval-buffer
                   ::init          'cider-connect)))

(use-package midje-mode
  :defer true
  :config (define-clojure-indent
            (comment 'defun)))

(use-package cider
  :defer true
  :init (progn (add-hook 'cider-mode-hook 'eldoc-mode)
               (setq nrepl-log-messages true)
               (setq cider-prefer-local-resources true)
               (setq nrepl-buffer-name-separator "/")
               (setq nrepl-buffer-name-show-port true)
               (setq cider-repl-use-clojure-font-lock true)
               (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
               (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
               (add-hook 'cider-repl-mode-hook 'subword-mode)
               (setq cider-cljs-lein-repl
                     "(do (require 'figwheel-sidecar.repl-api)
                          (figwheel-sidecar.repl-api/start-figwheel!)
                          (figwheel-sidecar.repl-api/cljs-repl))")))
