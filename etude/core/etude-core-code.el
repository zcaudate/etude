;; EGLOT for LSP
(use-package eglot :defer t)

;; Automatic completion
(define-key read-expression-map (kbd "TAB") 'completion-at-point)

(use-package company
  :ensure t
  :diminish 'company-mode
  :init   (progn (setq company-idle-delay 0.05)
                 (setq company-tooltip-limit 20)
                 (setq company-minimum-prefix-length 1)
                 (setq company-tooltip-flip-when-above t)
                 (setq company-show-numbers t))
  :config (progn (add-hook 'after-init-hook 'global-company-mode)
                 (define-key company-active-map
                   " "
                   (lambda ()
                     (interactive)
                     (company-abort)
                     (self-insert-command 1)))
                 (define-key company-active-map (kbd "<return>") nil)))

(use-package company-fuzzy :defer t)

;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

(provide 'etude-core-code)
