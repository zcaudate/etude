(use-package etude-lang)

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :init
  (setq rust-format-on-save t))

(use-package cargo
  :ensure t
  :mode "\\.rs\\'")

(use-package lsp-rust
  :ensure t
  :mode "\\.rs\\'"
  :after lsp-mode)

(provide 'etude-rust)
