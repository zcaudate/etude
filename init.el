;; use package.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
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
               etude-clojure))
