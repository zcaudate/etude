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

(use-package swiper :ensure t)

(use-package counsel :ensure t)

(use-package counsel-etags :ensure t)

(use-package counsel-projectile
  :ensure t
  :init (add-hook 'after-init-hook 'counsel-projectile-mode))

(use-package counsel-tramp :ensure t)

(use-package ace-window :ensure t)

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

(use-package treemacs :ensure t)
(setq treemacs-no-png-images t)
(use-package treemacs-projectile :ensure t)
(use-package treemacs-magit :ensure t)
