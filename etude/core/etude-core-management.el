(ns: etude-core-management)

(use-package recentf
  :defer true
  :config (progn (setq recentf-max-saved-items 200
                       recentf-max-menu-items 15)
                 (recentf-mode true)))

(use-package smex 
  :config (progn (smex-initialize)))

(use-package ivy
  :diminish 'ivy-mode
  :config (progn (ivy-mode true)
                 (setq ivy-initial-inputs-alist nil)
                 (setq ivy-use-virtual-buffers true)
                 (setq ivy-virtual-abbreviate 'abbreviate)     
                 (setq enable-recursive-minibuffers true)
                 (setq ivy-count-format "(%d/%d)")))

(use-package ivy-rich
  :config (ivy-rich-mode true))

(defun on/ivy-recentf-file ()
  (interactive)
  (let ((file (ivy-completing-read "Recent: " recentf-list nil t)))
    (when file
      (find-file file))))

(use-package projectile
  :diminish 'projectile-mode
  :config (setq projectile-completion-system 'ivy))

(use-package ag)

(use-package counsel)

(use-package counsel-projectile
  :init (add-hook 'after-init-hook 'counsel-projectile-mode))

(use-package swiper)

(use-package ace-window)

(use-package neotree
  :init   (add-hook 'neo-after-create-hook (lambda (_) (display-line-numbers-mode -1)))
  :config (progn (setq neo-theme 'nerd)
                 (setq neo-smart-open true)
                 (setq projectile-switch-project-action 'neotree-projectile-action)))

(defun on/start-screen ()
  (interactive)
  (dashboard-insert-startupify-lists)
  (redisplay)
  (switch-to-buffer "*dashboard*")
  (neotree-dir "~")
  (switch-to-buffer "*dashboard*"))

(use-package dashboard
  :init (progn (setq dashboard-startup-banner nil)
               (setq dashboard-items '((recents  . 15)
                                       (projects . 5)))
               (add-hook 'emacs-startup-hook 'on/start-screen)))

(defun on/neotree-toggle ()
  (interactive)
  (let ((cw (selected-window)))
    (neotree-toggle)
    (select-window cw)))

(defun on/neotree-projectile-root ()
  (interactive)
  (let ((project-dir (condition-case nil
                         (projectile-project-root)
                       (error "~")))
        (file-name   (condition-case nil
                         (buffer-file-name)
                       (error nil))))
    (let ((cw (selected-window)))
      (if project-dir
          (neo-global--open-dir project-dir))
      (select-window cw))))

(defun on/neotree-projectile-locate ()
  (interactive)
  (save-excursion
    (let ((cw (selected-window)))
      (let ((origin-buffer-file-name (buffer-file-name)))
        (when (and (fboundp 'projectile-project-p)
                   (projectile-project-p)
                   (fboundp 'projectile-project-root))
          (neo-global--open-dir (projectile-project-root))
          (neotree-find origin-buffer-file-name)
          (neotree-find (projectile-project-root))
          (recenter))))))

(provide 'etude-core-management)
