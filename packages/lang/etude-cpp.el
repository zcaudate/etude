(use-package flycheck
  :ensure t)

(use-package rtags
  :ensure t
  :config
  (progn
    ;; Start rtags upon entering a C/C++ file
    (add-hook
     'c-mode-common-hook
     (lambda () (if (not (is-current-file-tramp))
                    (rtags-start-process-unless-running))))
    (add-hook
     'c++-mode-common-hook
     (lambda () (if (not (is-current-file-tramp))
                    (rtags-start-process-unless-running))))
    ;; Flycheck setup
    (require 'flycheck-rtags)
    (defun my-flycheck-rtags-setup ()
      (flycheck-select-checker 'rtags)
      ;; RTags creates more accurate overlays.
      (setq-local flycheck-highlighting-mode nil)
      (setq-local flycheck-check-syntax-automatically nil))
    ;; c-mode-common-hook is also called by c++-mode
    (add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)
    ;; Keybindings
    (rtags-enable-standard-keybindings c-mode-base-map "\C-cr")
    )
  )
  
;; Use irony for completion
(use-package irony
  :ensure t
  :config
  (progn
    (add-hook
     'c-mode-common-hook
     (lambda () (if (not (is-current-file-tramp)) (irony-mode))))
    (add-hook
     'c++-mode-common-hook
     (lambda () (if (not (is-current-file-tramp)) (irony-mode))))
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    (use-package company-irony
      :ensure t
      :config
      (push 'company-irony company-backends)
      )
    )
  )

(use-package cmake-ide
  :ensure t)
  
    
(provide 'etude-cpp)
