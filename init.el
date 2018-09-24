
;; use package.el
(require 'package)

(progn (add-to-list 'package-archives
		    '("melpa" . "http://melpa.milkbox.net/packages/"))

       (when (< emacs-major-version 24)
	 (add-to-list 'package-archives
		      '("gnu" . "http://elpa.gnu.org/packages/"))) 
       (package-initialize))

;; use-package
(progn (setq package-enable-at-startup nil)
       (unless (package-installed-p 'use-package)
	 (package-refresh-contents)
	 (package-install 'use-package)))

(require 'use-package)

(defmacro use-packages (&rest args)
  (cons 'progn 
	(mapcar (lambda (pkg)
		  `(use-package ,pkg ,@(cdr args)))
		(car args))))

;; packages
(setq emacs-d (file-name-directory (or (buffer-file-name) 
                                       (file-chase-links load-file-name))))
(add-to-list 'load-path (concat emacs-d "packages"))
(add-to-list 'load-path (concat emacs-d "packages/core"))
(add-to-list 'load-path (concat emacs-d "packages/lang"))

(use-packages (etude-core
               etude-clang
	       etude-clojure
	       etude-elisp
	       etude-markdown
	       etude-rust))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (multiple-cursors breadcrumbs breadcrumb wgrep diminish ace-window ag ivy-rich counsel-projectile use-package-chords ivy-hydra counsel ivy exec-path-from-shell auto-highlight-symbol git-timemachine undo-tree dashboard lsp-flycheck lsp-rust cargo rust-mode lsp-ui magit etude-company etude-lang company-lsp lsp-mode neotree direx cider markdown-mode elisp-lint lispy hy-mode ess wakatime-mode midje-mode rainbow-delimiters paredit smartparens projectile diff-hl company ido-vertical-mode smex flx-ido ido-ubiquitous smart-mode-line solarized-theme use-package))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "#222"))))
 '(ivy-action ((t (:inherit font-lock-builtin-face :foreground "red2"))))
 '(ivy-current-match ((t (:background "dark green" :foreground "light green" :weight normal))))
 '(ivy-cursor ((t (:background "gray20" :foreground "white"))))
 '(ivy-highlight-face ((t (:inherit highlight :foreground "dark magenta"))))
 '(ivy-minibuffer-match-face-1 ((t (:foreground "dark cyan" :height 0.8))))
 '(ivy-minibuffer-match-highlight ((t (:inherit highlight :foreground "purple4"))))
 '(ivy-modified-buffer ((t (:inherit default :foreground "LightSalmon1"))))
 '(ivy-modified-outside-buffer ((t (:inherit default :foreground "DodgerBlue2"))))
 '(ivy-prompt-match ((t (:inherit default :foreground "firebrick")))))
           
;; Time tracking
(use-package wakatime-mode
  :ensure t
  :diminish wakatime-mode
  :config (progn
	    (setq wakatime-cli-path "/usr/local/bin/wakatime")
	    (setq wakatime-python-bin nil)
	    (global-wakatime-mode)))

(use-package diminish
  :ensure t
  :config (progn
	    (eval-after-load "git-gutter"  '(diminish 'git-gutter-mode))
	    (eval-after-load "waka"        '(diminish 'wakatime-mode))
	    (eval-after-load "undo-tree"   '(diminish 'undo-tree-mode))
	    (eval-after-load "paredit"     '(diminish 'paredit-mode))
	    (eval-after-load "ivy"         '(diminish 'ivy-mode))
	    (eval-after-load "projectile"  '(diminish 'projectile-mode))
	    (eval-after-load "company"     '(diminish 'company-mode))
	    (eval-after-load "smartparens" '(diminish 'smartparens-mode))))

(server-start)
