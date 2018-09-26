(ns: etude-module-native
  (:require
   etude-core))

;;
;; OPENCL
;;

(use-package opencl-mode
  :defer t
  :mode "\\.cl\\'")

;;
;; RUST
;;

(use-package rust-mode
  :defer t
  :mode "\\.rs\\'"
  :init
  (setq rust-format-on-save t))

(use-package cargo
  :defer t
  :mode "\\.rs\\'")

(use-package lsp-rust
  :defer t
  :mode "\\.rs\\'")

;;
;; MAKE
;;

;;
;; CMAKE
;;


;;
;; CLANG
;;

(use-package lsp-clangd
  :ensure t
  :load-path
  "<path-to-lsp-clangd>"
  :hook
  ((c-mode . lsp-clangd-c-enable)
   (c++-mode . lsp-clangd-c++-enable)
   (objc-mode . lsp-clangd-objc-enable)))
