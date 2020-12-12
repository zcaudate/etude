(ns: etude-module-native
  (:require
   etude-core))


;;
;; Java
;;

(setenv "JAVA_HOME" "/Library/Java/JavaVirtualMachines/jdk-10.0.2.jdk")
(require 'cedet)

(use-package jdee
  :defer true
  :config (setq jdee-server-dir (s-concat emacs-d "dev")))

;;
;; OPENCL
;;

(use-package opencl-mode
  :defer true
  :mode   "\\.cl\\'")

;;
;; RUST
;;

(use-package cargo
  :defer true)

(use-package rust-mode
  :defer true
  :mode   "\\.rs\\'"
  :init   (require 'cargo)
  :hook   cargo-minor-mode
  :config (progn (setq rust-format-on-save true)
                 (on/mode: [:rust rust-mode  "etude-module-native"]
                   ::init       'cargo-process-init
                   ::eval-file  'cargo-process-run)))

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

(use-package lsp-clangd
  :defer true
  :init  (when (equal system-type 'darwin)
           (setq lsp-clangd-executable "/usr/local/opt/llvm/bin/clangd"))
  :hook  ((c-mode . lsp-clangd-c-enable)
          (c++-mode . lsp-clangd-c++-enable)
          (objc-mode . lsp-clangd-objc-enable)))
