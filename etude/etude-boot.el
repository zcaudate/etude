(setq true t)
(setq false -1)

(add-to-list 'load-path (concat emacs-d "etude/core"))
(add-to-list 'load-path (concat emacs-d "etude/module"))
(add-to-list 'load-path (concat emacs-d "etude/mode"))
(add-to-list 'load-path (concat emacs-d "etude/lib"))

(require 'package)
(progn (add-to-list 'package-archives
		    '("melpa" . "http://melpa.org/packages/"))

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

;; DEFAULTS


;; Skip the default splash screen.
(setq inhibit-startup-message t)

;; Make sure we always use UTF-8.
;; (require 'iso-transl)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Emacs writes backup files to `filename~` by default. This is messy,
;; so let's tell it to write them to `~/.emacs.d/backup` instead.
;; If you have an accident, check this directory - you might get lucky.
(setq backup-directory-alist    '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20      ; how many of the newest versions to keep
      kept-old-versions 5)      ; and how many of the old

;; Automatically save buffers before launching M-x compile and friends,
;; instead of asking you if you want to save.
(setq compilation-ask-about-save nil)
(auto-save-mode t)

;; Make the selection work like most people expect.
(delete-selection-mode t)
(transient-mark-mode t)

;; Automatically update unmodified buffers whose files have changed.
(global-auto-revert-mode t)

;; XTERM Mouse support
(xterm-mouse-mode t)

(defun e/scroll-down-one ()
  (interactive)
  (scroll-down 1))

(defun e/scroll-up-one ()
  (interactive)
  (scroll-up 1))

(global-set-key [mouse-4] 'e/scroll-down-one)
(global-set-key [mouse-5] 'e/scroll-up-one)


;; Indentation
(setq-default indent-tabs-mode nil)


;; Fix vector intentation
(defun e/lisp-indent-function (indent-point state)
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (or (not (eq ?\( (char-before)))
            (and (elt state 2)
                 (not (and (looking-at "\\sw\\|\\s_")
                           (not (eq ?: (char-after)))))))
        ;; indent as data
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      ;; indent as function
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state)))))))

(setq-default lisp-indent-function 'e/lisp-indent-function)

(provide 'etude-boot)
