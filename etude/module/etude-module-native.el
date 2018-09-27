(ns: etude-module-native
  (:require
   etude-core))

;;
;; OPENCL
;;

(use-package opencl-mode
  :mode "\\.cl\\'")

;;
;; RUST
;;

(use-package cargo)

(use-package rust-mode
  :mode "\\.rs\\'"
  :config (progn (setq rust-format-on-save true)
                 (cargo-minor-mode true)
                 (on/mode: [:rust rust-mode]
                   "etude-module-native" 
                   ::init       'cargo-process-init
                   ::eval-file  'cargo-process-build
                   ::test       'cargo-process-test
                   ::run        'cargo-process-run)))

;;
;; MAKE
;;

(defun on/make-save-and-compile ()
  (interactive)
  (save-buffer)
  (compile "make -j4")
  (pop-to-buffer next-error-last-buffer))

;;
;; CMAKE
;;

(use-package cmake-mode
  :defer true
  :mode "\\CMakeLists.txt\\'" )

;;
;; CLANG
;;

(use-package c-mode
  :defer true)

(use-package lsp-clangd
  :defer true
  :init
  (when (equal system-type 'darwin)
    (setq lsp-clangd-executable "/usr/local/opt/llvm/bin/clangd"))
  :hook
  ((c-mode . lsp-clangd-c-enable)
   (c++-mode . lsp-clangd-c++-enable)
   (objc-mode . lsp-clangd-objc-enable)))
