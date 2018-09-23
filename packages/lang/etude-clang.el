(use-package etude-lang)

(use-package lsp-clangd
  :ensure t
  :load-path
  "<path-to-lsp-clangd>"
  :hook
  ((c-mode . lsp-clangd-c-enable)
   (c++-mode . lsp-clangd-c++-enable)
   (objc-mode . lsp-clangd-objc-enable)))

(provide 'etude-clang)
