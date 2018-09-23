(use-package etude-company)

(use-package flycheck
  :ensure t)

(use-package lsp-mode
  :ensure t
  :config
  (require 'lsp-mode))

(use-package lsp-ui
  :ensure t
  :config
  (require 'lsp-ui))

(use-package company-lsp
  :ensure t
  :config
  (progn
    (require 'company-lsp)
    (push 'company-lsp company-backends)
    (add-hook 'after-init-hook 'global-company-mode)))

(provide 'etude-lang)
