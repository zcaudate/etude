(use-package etude-projectile)

(use-package neotree
  :ensure t
  :config (progn (setq neo-theme 'nerd)
		 (setq neo-smart-open t)
		 (setq projectile-switch-project-action 'neotree-projectile-action)))

(defun neotree-toggle-current ()
  (interactive)
  (let ((cw (selected-window)))  ;; save current window
    (neotree-toggle)
    (select-window cw)))

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (condition-case nil
			 (projectile-project-root)
		       (error "~")))
	(file-name   (condition-case nil
			 (buffer-file-name)
		       (error nil))))
    (let ((cw (selected-window)))  ;; save current window
      (if project-dir
	  (neo-global--open-dir project-dir))
      (select-window cw))))

(defun neotree-locate-file ()
  "Refresh the NeoTree buffer."
  (interactive)
  (save-excursion
    (let ((cw (selected-window)))  ;; save current window
      (let ((origin-buffer-file-name (buffer-file-name)))
        (when (and (fboundp 'projectile-project-p)
                   (projectile-project-p)
                   (fboundp 'projectile-project-root))
          (neo-global--open-dir (projectile-project-root))
	  (neotree-find origin-buffer-file-name)
	  (neotree-find (projectile-project-root))
	  (recenter))))))

(add-hook 'neo-after-create-hook (lambda (_) (display-line-numbers-mode -1)))

(provide 'etude-neotree)
