;; Add shell defaults

(use-package exec-path-from-shell
  :ensure t
  :config (progn (exec-path-from-shell-initialize)))

;; VTerm support
(use-package vterm :ensure t
  :init (setq vterm-always-compile-module t))

;;
;; OSX
;;
(defun e/copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun e/paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

(if (eq system-type 'darwin)
    (progn
      (setq interprogram-cut-function 'e/paste-to-osx)
      (setq interprogram-paste-function 'e/copy-from-osx)))

;;
;;
;;

(provide 'etude-core-shell)
