(ns: etude-core-code
  (:require etude-core-base))

;; Automatic completion
(define-key read-expression-map (kbd "TAB") 'completion-at-point)

(defun on/complete-number ()
  "Forward to `company-complete-number'.
   Unless the number is potentially part of the candidate.
   In that case, insert the number."
  (interactive)
  (let: [k (this-command-keys)
         re (concat "^" company-prefix k)]
    (if (cl-find-if (lambda (s) (string-match re s))
                    company-candidates)
        (self-insert-command t)
      (company-complete-number (string-to-number k)))))

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

(use-package company-fuzzy :ensure t)

(use-package magit
  :ensure t)

(defun on/magit-add-commit-push ()
  (interactive)
  (save-some-buffers))

(use-package git-gutter
  :ensure t
  :diminish 'git-gutter-mode
  :config   (global-git-gutter-mode t))

(use-package git-timemachine :ensure t)

(use-package eglot :ensure t)
