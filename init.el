;; Boot

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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
(require 'etude-module-text)
;; (require 'etude-module-native)

(server-force-delete)
(server-start)


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
 '(nord-comment-brightness 20)
 '(nord-region-highlight "\"frost\"")
 '(nord-uniform-mode-lines t)
 '(package-selected-packages
   (quote
    (counsel-tramp auto-highlight-symbol cider clojure-mode company counsel-codesearch counsel-etags counsel-projectile dashboard magit midje-mode neotree nord-theme paredit rainbow-delimiters smart-mode-line smartparens smex use-package))))
