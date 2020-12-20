

;;
;; MAKE
;;

(defun e/make-save-and-compile ()
  (interactive)
  (save-buffer)
  (compile "make -j4")
  (pop-to-buffer next-error-last-buffer))

;;
;; CMAKE
;;

(use-package cmake-mode
  :defer t
  :mode "\\CMakeLists.txt\\'" )

;;
;; OPENCL
;;

(use-package opencl-mode
  :defer t
  :mode   "\\.cl\\'")

;;
;; CLANG
;;

(use-package lsp-clangd
  :defer t
  :init  (when (equal system-type 'darwin)
           (setq lsp-clangd-executable "/usr/local/opt/llvm/bin/clangd"))
  :hook  ((c-mode . lsp-clangd-c-enable)
          (c++-mode . lsp-clangd-c++-enable)
          (objc-mode . lsp-clangd-objc-enable)))
