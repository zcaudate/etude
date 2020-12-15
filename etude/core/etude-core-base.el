(ns: etude-core-base)

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

(global-set-key [mouse-4] '(lambda ()
                             (interactive)
                             (scroll-down 1)))
(global-set-key [mouse-5] '(lambda ()
                             (interactive)
                             (scroll-up 1)))


;; Indentation
(setq-default indent-tabs-mode nil)

;; Lisp Modes
(use-package smartparens
  :ensure t
  :diminish 'smartparens-mode
  :config (progn (require 'smartparens-config)
                 (smartparens-global-mode t)
                 (show-paren-mode t)))

(use-package paredit
  :ensure t
  :diminish 'paredit-mode
  :init  (define-key read-expression-map (kbd "TAB") 'completion-at-point))

(defun on/paredit-copy-as-kill ()
  (interactive)
  (paredit-kill)
  (save-excursion (yank)))

(use-package rainbow-delimiters
  :ensure t)

(use-package undo-tree
  :ensure t
  :diminish 'undo-tree-mode
  :config (global-undo-tree-mode t))
