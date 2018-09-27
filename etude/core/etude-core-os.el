(ns: etude-core-os)

;; Add shell defaults
(comment:
    (use-package exec-path-from-shell
      :defer t
      :config (progn (exec-path-from-shell-initialize))))

(provide 'etude-core-shell)
