(use-package clojure-mode
  :init (progn (add-hook 'clojure-mode-hook 'smartparens-strict-mode)
               (add-hook 'clojure-mode-hook 'paredit-mode)
               (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)))

(use-package midje-mode)
  
(use-package cider
  :init (progn  (add-hook 'cider-mode-hook 'eldoc-mode)
          	    (setq nrepl-log-messages t)
          	    (setq cider-prefer-local-resources t)
          	    (setq nrepl-buffer-name-separator "/")
          	    (setq nrepl-buffer-name-show-port t)
	              (setq cider-repl-use-clojure-font-lock t)
	              (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
	              (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
	              (add-hook 'cider-repl-mode-hook 'subword-mode))
  :config  (progn (mode:
		   (::clojure clojure-mode
			      "etude-lang-clojure")
		   ::eval-cursor   'cider-eval-last-sexp
		   ::eval-file     'cider-eval-buffer
		   ::init          'cider-connect)))

(setq cider-cljs-lein-repl
      "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")

(provide 'etude-lang-clojure)
