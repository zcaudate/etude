(ns: etude-core-shell)

;; Add shell defaults
(use-package exec-path-from-shell
  :config (progn (exec-path-from-shell-initialize)))

(provide 'etude-core-shell)
