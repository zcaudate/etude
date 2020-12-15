(ns: etude-core-os)

;; Add shell defaults

(use-package exec-path-from-shell
  :ensure t
  :config (progn (exec-path-from-shell-initialize)))


;; EShell Support
(use-package pcomplete-extension :ensure t)
(use-package fish-mode :ensure t)
(use-package fish-completion :ensure t)


;; VTerm support
(use-package vterm :ensure t)
(use-package multi-vterm :ensure t)

;;
;; OSX
;;
(defun on/copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun on/paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

(if (eq system-type 'darwin)
    (progn
      (setq interprogram-cut-function 'on/paste-to-osx)
      (setq interprogram-paste-function 'on/copy-from-osx)))

;;
;;
;;

(provide 'etude-core-shell)
