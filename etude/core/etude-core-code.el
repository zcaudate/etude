(ns: etude-core-code)

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
        (self-insert-command true)
      (company-complete-number (string-to-number k)))))

(use-package company
  :diminish 'company-mode
  :init   (progn (setq company-idle-delay 0.05)
                 (setq company-tooltip-limit 20)
                 (setq company-minimum-prefix-length 1)
                 (setq company-tooltip-flip-when-above true)
                 (setq company-show-numbers true))
  :config (progn (add-hook 'after-init-hook 'global-company-mode)
                 (let: [map company-active-map]
                   (mapc
                    (lambda (x)
                      (define-key map (format "%d" x) 'on/complete-number))
                    (number-sequence 0 9))
                   (define-key map " " (lambda ()
                                         (interactive)
                                         (company-abort)
                                         (self-insert-command 1)))
                   (define-key map (kbd "<return>") nil))))

(use-package flycheck
  :defer true)

(use-package lsp-mode
  :defer true
  :config (progn (require 'lsp-mode)))

(use-package lsp-ui
  :defer true
  :config (progn (require 'lsp-ui)))

(use-package company-lsp
  :defer true
  :init (progn (require 'company-lsp)
               (push 'company-lsp company-backends)))
 
(use-package magit
  :defer true)

(defun on/magit-add-commit-push ()
  (interactive)
  (save-some-buffers))

(use-package git-timemachine
  :defer true)

(use-package git-gutter
  :defer true
  :diminish 'git-gutter-mode
  :config   (global-git-gutter-mode true))

(use-package wakatime-mode
  :diminish 'wakatime-mode
  :config   (progn (setq wakatime-cli-path "/usr/local/bin/wakatime")
	           (setq wakatime-python-bin nil)
	           (global-wakatime-mode true)))

(provide 'etude-core-code)
