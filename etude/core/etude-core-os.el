(ns: etude-core-os)

;; Add shell defaults

(use-package exec-path-from-shell
  :ensure t
  :config (progn (exec-path-from-shell-initialize)))

(defun on/copy-from-osx ()
    (shell-command-to-string "pbpaste"))

(defun on/paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

(setq interprogram-cut-function 'on/paste-to-osx)
(setq interprogram-paste-function 'on/copy-from-osx)

(use-package fish-mode :ensure t)

(provide 'etude-core-shell)
