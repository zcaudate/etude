;; use package.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))) 
(package-initialize)

;; use-package
(setq package-enable-at-startup nil)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
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
               etude-elisp
               etude-clojure
			   etude-cpp))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (lispy hy-mode ess wakatime-mode company-irony flycheck-irony flycheck-rtags company-rtags rtags midje-mode rainbow-delimiters paredit smartparens projectile diff-hl company ido-vertical-mode smex flx-ido ido-ubiquitous smart-mode-line solarized-theme use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Time tracking
;;(global-wakatime-mode)

