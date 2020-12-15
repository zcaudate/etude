(ns: etude-core-management
  (:require etude-core-base))

(use-package recentf
  :defer t
  :config (progn (setq recentf-max-saved-items 200
                       recentf-max-menu-items 15)
                 (recentf-mode t)))

(use-package smex
  :ensure t
  :config (progn (smex-initialize)))

(use-package ivy
  :ensure t
  :diminish 'ivy-mode
  :config (progn (ivy-mode t)
                 (setq ivy-initial-inputs-alist nil)
                 (setq ivy-use-virtual-buffers t)
                 (setq ivy-virtual-abbreviate 'abbreviate)     
                 (setq enable-recursive-minibuffers t)
                 (setq ivy-count-format "(%d/%d)")))

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

(use-package ivy-rich
  :ensure t
  :config (ivy-rich-mode t))

(use-package ace-window :ensure t)

(defun on/start-screen ()
  (interactive)
  (dashboard-insert-startupify-lists)
  (redisplay)
  (switch-to-buffer "*dashboard*"))

(use-package dashboard
  :ensure t
  :init (progn (setq dashboard-startup-banner nil)
               (setq dashboard-items '((recents  . 15)
                                       (projects . 5)))
               (add-hook 'emacs-startup-hook 'on/start-screen)))

(use-package treemacs :ensure t
  :init (setq treemacs-no-png-images t))

(use-package treemacs-projectile :ensure t)
(use-package treemacs-magit :ensure t)

(use-package doom-modeline :ensure t
  :init (doom-modeline-mode))


(use-package multiple-cursors :ensure t)

(defhydra hydra-multiple-cursors (:hint nil)
  "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search
 [Click] Cursor at point       [_q_] Quit"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("s" mc/mark-all-in-region-regexp :exit t)
  ("0" mc/insert-numbers :exit t)
  ("A" mc/insert-letters :exit t)
  ("<mouse-1>" mc/add-cursor-on-click)
  ;; Help with click recognition in this hydra
  ("<down-mouse-1>" ignore)
  ("<drag-mouse-1>" ignore)
  ("q" nil))
