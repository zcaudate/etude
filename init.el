;; Boot

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(setq gc-cons-threshold (* 50 1000 1000))

(progn (setq emacs-d
             (file-name-directory
              (or (buffer-file-name) 
                  (file-chase-links load-file-name))))
       (add-to-list 'load-path (concat emacs-d "etude"))
       (require 'etude-boot))

(setq use-package-always-ensure nil)

;; Core
(require 'etude-lang)
(require 'etude-core)
(require 'etude-module-lisp)
(require 'etude-module-docker)
(require 'etude-module-text)
(require 'etude-module-org)
(require 'etude-module-apps)


;;(require 'etude-module-jvm)
;; (require 'etude-module-native)

;;(server-force-delete)
;;(server-start)

(setq gc-cons-threshold (* 2 1000 1000))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-link ((t (:foreground "blue" :underline nil)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(nord))
 '(custom-safe-themes
   '("e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "bf387180109d222aee6bb089db48ed38403a1e330c9ec69fe1f52460a8936b66" "37768a79b479684b0756dec7c0fc7652082910c37d8863c35b702db3f16000f8" "9685cefcb4efd32520b899a34925c476e7920725c8d1f660e7336f37d6d95764" default))
 '(doom-modeline-icon nil)
 '(global-linum-mode nil)
 '(org-babel-load-languages
   '((emacs-lisp . t)
     (js . t)
     (md . t)
     (shell . t)
     (dot . t)
     (gnuplot . t)
     (java . t)))
 '(org-mouse-1-follows-link 'double)
 '(org-support-shift-select 'always)
 '(package-selected-packages
   '(no-littering org-cliplink diff-hl wgrep gdscript-mode eglot git-timemachine bufler multi-vterm w3m company-fuzzy doom-modeline ivy-rich dired-rsync graphviz-dot-mode git-gutter treemacs-projectile treemacs-magit markdown-mode treemacs fish-completion pcomplete-extension fish-mode undo-tree magit dockerfile-mode midje-mode cider nord-theme dashboard neotree ace-window counsel-tramp counsel-projectile counsel-etags counsel projectile ivy company rainbow-delimiters paredit smartparens hydra f ht dash s use-package)))
