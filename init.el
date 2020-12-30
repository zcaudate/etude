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
 '(package-selected-packages
   '(ob-async plain-org-wiki org-cliplink auto-highlight-symbol doom-modeline nord-theme ivy-rich counsel-projectile counsel swiper wgrep ivy smex treemacs-projectile treemacs dashboard projectile ranger dired-filter dired-collapse dired-subtree no-littering impatient-mode yasnippet goto-chg iedit undo-tree goggles bufler ace-window which-key tldr helpful multi-vterm vterm exec-path-from-shell midje-mode cider rainbow-delimiters paredit smartparens git-gutter graphviz-dot-mode markdown-mode dockerfile-mode yaml-mode company pretty-hydra hydra f ht dash s use-package)))
