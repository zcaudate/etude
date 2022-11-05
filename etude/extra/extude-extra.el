
(use-package magit :ensure t
  :config (add-hook 'git-commit-setup-hook
                    (lambda ()
                      (add-hook 'with-editor-post-finish-hook
                                (lambda ()
                                  (call-interactively #'magit-push-current-to-upstream))
                                t t))))



(use-package git-timemachine :ensure t)

(use-package diff-hl :ensure t)

(pretty-hydra-define e/menu-fn::git-timemachine-menu
  (:title "Git Timemachine" 
   :body-pre (git-timemachine)
   :quit-key "z"  :exit nil :foreign-keys run)
  ("Travel"
   (("p" git-timemachine-show-previous-revision "Prev")
    ("n" git-timemachine-show-next-revision  "Next")
    ("j" git-timemachine-show-nth-revision "To Nth")
    ("h" git-timemachine-help "Help")
    ("X" git-timemachine-quit "Exit"))
   ""
   (("L" git-timemachine-blame)
    ("T" git-timemachine-toggle)
    ("C" git-timemachine-show-commit)
    ("K" git-timemachine-kill-revision)
    ("B" git-timemachine-switch-branch))))

(defun e/git-timemachive-mode-menu ()
  (interactive)
  (if (eq hydra-curr-map e/menu-fn::git-timemachine-menu/keymap)
      (hydra-keyboard-quit)
    (e/menu-fn::git-timemachine-menu/body))))


;; Documentation
(use-package helpful :ensure t)

(use-package tldr :ensure t)

(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")

(use-package dockerfile-mode
  :ensure t)
  
(use-package graphviz-dot-mode :ensure t)

;; EGLOT for LSP
(use-package eglot :ensure t :ensure t)



(use-package ace-window :ensure t)


;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

(use-package company-restclient :ensure t
  :config (add-to-list 'company-backends 'company-restclient))

;;
;; Web + Web Server
;;

(use-package impatient-mode :ensure t
  :config (setq impatient-mode-delay 3))

(use-package restclient :ensure t)

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


(use-package goto-chg :ensure t)

(require 'color-rg)

   
