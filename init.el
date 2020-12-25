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
 '(custom-comment ((t (:foreground "color-115"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "color-243"))))
 '(font-lock-comment-face ((t (:foreground "color-243"))))
 '(font-lock-doc-face ((t (:foreground "color-243"))))
 '(goggles-added ((t (:background "brightblack"))))
 '(goggles-changed ((t (:background "brightblack"))))
 '(goggles-removed ((t (:extend t :background "brightblack"))))
 '(hydra-face-blue ((t (:foreground "blue" :weight bold))))
 '(org-block-begin-line ((t (:foreground "color-99"))))
 '(org-block-end-line ((t (:foreground "color-92"))))
 '(org-code ((t (:foreground "brightgreen"))))
 '(org-document-info-keyword ((t (:foreground "color-243" :weight bold))))
 '(org-link ((t (:foreground "brightblue" :underline nil))))
 '(org-meta-line ((t (:foreground "yellow")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(custom-safe-themes
   '("37768a79b479684b0756dec7c0fc7652082910c37d8863c35b702db3f16000f8" default))
 '(doom-modeline-icon nil)
 '(global-linum-mode nil)
 '(nord-comment-brightness 20)
 '(org-babel-load-languages
   '((emacs-lisp . t)
     (js . t)
     (python . t)
     (shell . t)
     (dot . t)
     (gnuplot . t)
     (java . t)))
 '(org-mouse-1-follows-link 'double)
 '(org-support-shift-select 'always)
 '(org-todo-keywords '((sequence "TODO" "DONE" "RESULTS")))
 '(package-selected-packages
   '(quelpa-use-package quelpa ob-async plain-org-wiki org-roam yasnippet ivy-posframe iedit auto-highlight-symbol counsel-dash goto-chg tldr symbol-overlay dired-filter dired-collapse dired-subtree goggles helpful no-littering org-cliplink diff-hl wgrep eglot git-timemachine bufler multi-vterm w3m company-fuzzy doom-modeline ivy-rich dired-rsync graphviz-dot-mode git-gutter treemacs-projectile treemacs-magit markdown-mode treemacs fish-completion pcomplete-extension fish-mode undo-tree magit dockerfile-mode midje-mode cider nord-theme dashboard neotree ace-window counsel-tramp counsel-projectile counsel-etags counsel projectile ivy company rainbow-delimiters paredit smartparens hydra f ht dash s use-package)))
