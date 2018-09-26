(setq true t)
(setq false -1)

(add-to-list 'load-path (concat emacs-d "etude/core"))
(add-to-list 'load-path (concat emacs-d "etude/module"))

(require 'package)
(progn (add-to-list 'package-archives
		    '("melpa" . "http://melpa.milkbox.net/packages/"))

       (when (< emacs-major-version 24)
	 (add-to-list 'package-archives
		      '("gnu" . "http://elpa.gnu.org/packages/"))) 
       (package-initialize))

(add-hook 'emacs-startup-hook
         (lambda ()
           (message "Emacs ready in %s with %d garbage collections."
                    (format "%.2f seconds"
                            (float-time
                             (time-subtract after-init-time before-init-time)))
                    gcs-done)))

;; use-package
(progn (setq package-enable-at-startup nil)
       (unless (package-installed-p 'use-package)
	        (package-refresh-contents)
	        (package-install 'use-package)))

(require 'use-package)
(provide 'etude-boot)
