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
       (add-to-list 'load-path (concat emacs-d "etude")))

(setq use-package-always-ensure nil)

;; Core
(require 'etude-boot)
(require 'etude-lang)
(require 'etude-core)
(require 'etude-bindings)

(require 'etude-module-org)

(setq gc-cons-threshold (* 2 1000 1000))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(goggles-added ((t (:background "brightblack"))))
 '(goggles-changed ((t (:background "brightblack"))))
 '(goggles-removed ((t (:extend t :background "brightblack"))))
 '(org-link ((t (:foreground "brightblue" :underline nil)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(doom-modeline-icon nil)
 '(global-linum-mode nil)
 '(org-babel-load-languages
   '((emacs-lisp . t)
     (js . t)
     (shell . t)
     (dot . t)
     (gnuplot . t)
     (java . t)))
 '(org-mouse-1-follows-link 'double)
 '(org-support-shift-select 'always)
 '(package-selected-packages
   '(ivy-posframe iedit auto-highlight-symbol counsel-dash annotate goto-chg tldr symbol-overlay dired-filter dired-collapse dired-subtree goggles helpful no-littering org-cliplink diff-hl wgrep eglot git-timemachine bufler multi-vterm w3m company-fuzzy doom-modeline ivy-rich dired-rsync graphviz-dot-mode git-gutter treemacs-projectile treemacs-magit markdown-mode treemacs fish-completion pcomplete-extension fish-mode undo-tree magit dockerfile-mode midje-mode cider nord-theme dashboard neotree ace-window counsel-tramp counsel-projectile counsel-etags counsel projectile ivy company rainbow-delimiters paredit smartparens hydra f ht dash s use-package)))
