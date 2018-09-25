;; Boot
(setq gc-cons-threshold (* 50 1000 1000))

(progn (setq emacs-d
	     (file-name-directory
	      (or (buffer-file-name) 
                  (file-chase-links load-file-name))))
       (add-to-list 'load-path (concat emacs-d "etude"))
       (require 'etude-boot))

(setq use-package-always-ensure nil)

;; Core
(require 'etude-global)
(require 'etude-core)

(require 'etude-lang-emacs-lisp)
(require 'etude-lang-clojure)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (f ht esup counsel-dash wgrep wakatime-mode use-package-chords undo-tree solarized-theme smex smartparens smart-mode-line rainbow-delimiters paredit neotree multiple-cursors midje-mode magit lsp-ui lsp-rust lsp-clangd ivy-rich ivy-hydra git-timemachine git-gutter exec-path-from-shell diminish diff-hl dashboard counsel-projectile company-lsp cargo auto-highlight-symbol ace-window))))
    
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "#222"))))
 '(ivy-action ((t (:inherit font-lock-builtin-face :foreground "red2"))))
 '(ivy-current-match ((t (:background "dark green" :foreground "light green" :weight normal))))
 '(ivy-cursor ((t (:background "gray20" :foreground "white")))))

(setq gc-cons-threshold (* 2 1000 1000))
