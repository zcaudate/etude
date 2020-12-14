(ns: etude-core-management
  (:require etude-core-base))

(use-package recentf
  :defer true
  :config (progn (setq recentf-max-saved-items 200
                       recentf-max-menu-items 15)
                 (recentf-mode true)))

(use-package smex
  :ensure t
  :config (progn (smex-initialize)))

(use-package ivy
  :ensure t
  :diminish 'ivy-mode
  :config (progn (ivy-mode true)
                 (setq ivy-initial-inputs-alist nil)
                 (setq ivy-use-virtual-buffers true)
                 (setq ivy-virtual-abbreviate 'abbreviate)     
                 (setq enable-recursive-minibuffers true)
                 (setq ivy-count-format "(%d/%d)")))

;; (use-package ivy-rich
;;   :ensure t
;;   :config (ivy-rich-mode true))

(defun on/ivy-recentf-file ()
  (interactive)
  (let: [file (ivy-completing-read "Recent: " recentf-list nil true)]
    (if file (find-file file))))

(use-package projectile
  :ensure t
  :diminish 'projectile-mode
  :config (setq projectile-completion-system 'ivy))

(use-package ag :ensure t)

(use-package counsel :ensure t)

(use-package counsel-etags :ensure t)

(use-package counsel-projectile
  :ensure t
  :init (add-hook 'after-init-hook 'counsel-projectile-mode))

(use-package counsel-tramp :ensure t)

(use-package swiper :ensure t)

(use-package ace-window :ensure t)

(defun on/neotree-toggle ()
  (interactive)
  (let: [cw (selected-window)]
    (neotree-toggle)
    (select-window cw)))

(defun on/neotree-projectile-root ()
  (interactive)
  (let: [project-dir (condition-case nil
                         (projectile-project-root)
                       (error "~"))
         file-name   (condition-case nil
                         (buffer-file-name)
                       (error nil))
         cw  (selected-window)]
    (neotree-refresh)
    (if project-dir
        (neo-global--open-dir project-dir))
    (select-window cw)))

(defun on/neotree-projectile-locate ()
  (interactive)
  (let: [cw (selected-window)
         bname (buffer-file-name)]
    (if (equal " *NeoTree*" (buffer-name (window-buffer cw)))
        (progn (select-window (previous-window))
               ;;(neotree-toggle)
               )
      (save-excursion
        (when (and (fboundp 'projectile-project-p)
                   (projectile-project-p)
                   (fboundp 'projectile-project-root))
          (neotree-refresh)
          (neo-global--open-dir (projectile-project-root))
          (neotree-find bname)
          (neotree-find (projectile-project-root))
          (recenter))))))

(use-package neotree
  :ensure t
  :commands neotree-mode
  :init   (add-hook 'neo-after-create-hook
                    (lambda (_)
                      (display-line-numbers-mode false)
                      (define-key neotree-mode-map (kbd "o") 'neotree-enter)
                      (define-key neotree-mode-map (kbd "n") 'neotree-create-node)
                      (define-key neotree-mode-map (kbd "r") 'neotree-rename-node)
                      (define-key neotree-mode-map (kbd "d") 'neotree-delete-node)))
  :config (progn (setq neo-theme 'nerd)
                 (setq neo-smart-open true)
                 (setq projectile-switch-project-action 'neotree-projectile-action)
                 (setq-default neo-show-hidden-files true)))

(defun on/start-screen ()
  (interactive)
  (dashboard-insert-startupify-lists)
  (redisplay)
  (switch-to-buffer "*dashboard*")
  ;;(neotree-dir "~")
  (switch-to-buffer "*dashboard*"))

(use-package dashboard
  :ensure t
  :init (progn (setq dashboard-startup-banner nil)
               (setq dashboard-items '((recents  . 15)
                                       (projects . 5)))
               (add-hook 'emacs-startup-hook 'on/start-screen)))
