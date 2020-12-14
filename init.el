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
(require 'etude-module-jvm)
(require 'etude-module-docker)
(require 'etude-module-text)
;; (require 'etude-module-native)

;;(server-force-delete)
;;(server-start)

(setq gc-cons-threshold (* 2 1000 1000))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(nord-comment-brightness 40)
 '(nord-region-highlight "\"forst\"")
 '(nord-uniform-mode-lines t)
 '(package-selected-packages
   '(fish-mode undo-tree magit ag docker-compose-mode docker-tramp docker dockerfile-mode midje-mode cider auto-highlight-symbol nord-theme dashboard neotree ace-window counsel-tramp counsel-projectile counsel-etags counsel projectile ivy smex company rainbow-delimiters paredit smartparens hydra f ht dash s use-package)))
