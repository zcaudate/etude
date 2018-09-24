(use-package clojure-mode
  :ensure t
  :config
  (progn
    (add-hook 'clojure-mode-hook 'smartparens-strict-mode)
    (add-hook 'clojure-mode-hook 'paredit-mode)
    (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)))

(use-package midje-mode
  :ensure t)

(defun open-clojure-config ()
  (interactive)
  (find-file "~/.emacs.d/packages/lang/etude-clojure.el"))

(defun paredit-forward-top-level ()
  (interactive))

(defun paredit-backward-top-level ()
  (interactive))

(use-package cider
  :ensure t
  :config (progn
	    (add-hook 'cider-mode-hook 'eldoc-mode)
	    (setq nrepl-log-messages t)
	    (setq cider-prefer-local-resources t)
	    (setq nrepl-buffer-name-separator "/")
	    (setq nrepl-buffer-name-show-port t)
	    
	    (setq cider-repl-use-clojure-font-lock t)
	    (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
	    (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
	    (add-hook 'cider-repl-mode-hook 'subword-mode))

  :bind (:map clojure-mode-map
	 ("C-e" . cider-eval-last-sexp)
	 ("M-e" . cider-load-buffer)
	 ("M-8" . cider-connect)
	 ("M-0" . open-clojure-config)
	 ("<M-up>" . paredit-backward-top-level)
	 ("<M-down>" . paredit-forward-top-leve)
	 ("<M-left>" . paredit-backward)
	 ("<M-right>" . paredit-forward))) 

(setq cider-cljs-lein-repl
      "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")

(provide 'etude-clojure)
