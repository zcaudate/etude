

;;
;; RUST
;;

(use-package cargo
  :defer t)

(use-package rust-mode
  :defer t
  :mode   "\\.rs\\'"
  :init   (require 'cargo)
  :hook   cargo-minor-mode
  :config (progn (setq rust-format-on-save t)
                 (e/mode [:rust rust-mode  "etude-module-native"]
                   ::init       'cargo-process-init
                   ::eval-file  'cargo-process-run)))

(provide etude-module-rust)