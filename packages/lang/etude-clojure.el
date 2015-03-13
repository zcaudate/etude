(use-package etude-programming)

(use-package clojure-mode
  :ensure t
  :config
  (progn
    (add-hook 'clojure-mode-hook 'smartparens-strict-mode)
    (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)))

(use-package cider
  :ensure t
  :config
  (progn
    (add-hook 'cider-mode-hook 'eldoc-mode)
    (setq nrepl-log-messages t)
    (setq cider-prefer-local-resources t)
    (setq nrepl-buffer-name-separator "/")
    (setq nrepl-buffer-name-show-port t)
    
    (setq cider-repl-use-clojure-font-lock t)
    (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
    (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'cider-repl-mode-hook 'subword-mode)))
    
(provide 'etude-clojure)
