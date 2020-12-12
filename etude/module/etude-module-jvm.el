(ns: etude-module-jvm
  (:require etude-core))

;;
;; Clojure
;;

(use-package midje-mode
  :defer true
  :config (define-clojure-indent
            (comment 'defun)))

(use-package cider
  :defer true
  :init (progn (setq nrepl-log-messages true)
               (setq nrepl-buffer-name-separator "/")
               (setq nrepl-buffer-name-show-port true)
               (setq cider-prefer-local-resources true)
               (setq cider-repl-use-clojure-font-lock true)
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
  :defer true
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
