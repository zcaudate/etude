(require 'etude-core-shell)

;; Documentation
(use-package helpful :ensure t)

(use-package tldr :ensure t)

;;
;; Movement/Cut/Paste/Undo
;;

(use-package which-key :ensure t
  :init (progn (setq which-key-sort-order 
                     'which-key-local-then-key-order)))


(use-package ace-window :ensure t)

(use-package bufler :ensure t)

(use-package goggles
  :ensure t
  :init   (setq-default goggles-pulse t)
  :config (goggles-mode))

(use-package undo-tree
  :ensure t
  :diminish 'undo-tree-mode
  :config (global-undo-tree-mode t))

(use-package iedit :ensure t)

(use-package goto-chg :ensure t)

(use-package annotate :ensure t)

(use-package yasnippet :ensure t
  :init (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets")))

;;
;; Web + Web Server
;;

(use-package impatient-mode :ensure t
  :config (setq impatient-mode-delay 3))

;;
;; File Management
;;

(use-package no-littering :ensure t)

(use-package recentf
  :defer t
  :config (progn (setq recentf-max-saved-items 200
                       recentf-max-menu-items 15)
                 (recentf-mode t)))
;
(use-package dired-subtree :ensure t
  :bind (:map dired-mode-map
         ("i" . dired-subtree-toggle)))

(use-package dired-collapse :ensure t
  :hook (dired-mode . dired-collapse-mode))

(use-package dired-filter :ensure t
  :hook (dired-mode . (lambda () (dired-filter-group-mode)
                        (dired-filter-by-garbage)))
  :custom
  (dired-garbage-files-regexp
   "*.\\(aux\\|bak\\|dvi\\|log\\|orig\\|rej\\|toc\\|out\\)")
  (dired-filter-group-saved-groups
   '(("default"
      ("Org"    (extension "org"))
      ("Executables" (exexutable))
      ("Directories" (directory))
      ("PDF"    (extension "pdf"))
      ("LaTeX"  (extension "tex" "bib"))
      ("Images" (extension "png"))
      ("Code"   (extension "java" "el" "clj" "js"))
      ("Archives"(extension "zip" "rar" "gz" "bz2" "tar"))))))

(defun my-demo-function ()
  (setq last-counsel-M-x-command (caar command-history)))

(use-package ranger :ensure t
  :init (progn (setq ranger-cleanup-on-disable t)
               (setq ranger-cleanup-eagerly t)
               (setq ranger-show-hidden t)
               (setq ranger-modify-header t)
               (setq ranger-parent-depth 0)
               (setq ranger-footer-delay 0.2)
               (setq ranger-preview-delay 0.040)
               (setq ranger-show-literal nil)
               (setq ranger-max-preview-size 1)
               (setq ranger-dont-show-binary t))
  :hook (range-mode . which-key-mode))

;;
;; Project Management
;;

(use-package projectile
  :ensure t
  :diminish 'projectile-mode
  :config (setq projectile-completion-system 'ivy))

(defun e/start-screen ()
  (interactive)
  (dashboard-insert-startupify-lists)
  (redisplay)
  (switch-to-buffer "*dashboard*")
  (define-key dashboard-mode-map [down-mouse-1] 'nil))

(use-package dashboard
  :ensure t
  :init (progn (setq dashboard-startup-banner nil)
               (setq dashboard-items '((recents  . 15)
                                       (projects . 5)))
               (add-hook 'emacs-startup-hook 'e/start-screen)))

(use-package treemacs :ensure t
  :init (setq treemacs-no-png-images t))

(use-package treemacs-projectile :ensure t)
(use-package treemacs-magit :ensure t)
          
;;
;; Search
;;

(use-package smex :ensure t)

(use-package ivy
  :ensure t
  :diminish 'ivy-mode
  :config (progn (ivy-mode t)
                 (setq ivy-initial-inputs-alist nil)
                 (setq ivy-use-virtual-buffers t)
                 (setq ivy-virtual-abbreviate 'abbreviate)     
                 (setq enable-recursive-minibuffers t)
                 (setq ivy-count-format "(%d/%d)")))

(use-package wgrep :ensure t)

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


(provide 'etude-core-management)
