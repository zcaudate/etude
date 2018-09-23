;; Enable ido-mode.
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-url-at-point t
      ido-max-prospects 10
      ido-use-virtual-buffers t)

;; Make sure ido is really everywhere.
(use-package ido-completing-read+
  :ensure t
  :init (ido-ubiquitous-mode))

;; Better fuzzy-search for ido
(use-package flx-ido
  :ensure t)


;; Use smex to provide ido-like interface for M-x
(use-package smex
  :ensure t
  :init (progn
          (smex-initialize)
          (global-set-key (kbd "M-x") 'smex)
          (global-set-key (kbd "M-X") 'smex-major-mode-commands)
          (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)))

;; Vertical ido.
(use-package ido-vertical-mode
  :ensure t
  :init (ido-vertical-mode))
  
;; Bind `~` to go to homedir when in ido-find-file.
;; From http://whattheemacsd.com/setup-ido.el-02.html
(add-hook 'ido-setup-hook
 (lambda ()
   ;; Go straight home
   (define-key ido-file-completion-map
     (kbd "~")
     (lambda ()
       (interactive)
       (if (looking-back "/")
           (insert "~/")
         (call-interactively 'self-insert-command))))))

(provide 'etude-ido)
