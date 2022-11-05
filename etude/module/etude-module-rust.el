

;;
;; RUST
;;

(use-package cargo
  :ensure t)

(use-package rust-mode
  :ensure t
  :mode   "\\.rs\\'"
  :init   (require 'cargo)
  :hook   cargo-minor-mode
  :config (progn (setq rust-format-on-save t)
                 (eta-modal[:rust rust-mode  "etude-module-native"]
                   ::init       'cargo-process-init
                   ::eval-file  'cargo-process-run)))

(provide etude-module-rust)