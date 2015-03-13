;; Skip the default splash screen.
(setq inhibit-startup-message t)

;; Add /usr/local/bin to PATH variable
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (cons "/usr/local/bin" exec-path))

;; Make sure we always use UTF-8.
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(load-library "iso-transl")

;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Emacs writes backup files to `filename~` by default. This is messy,
;; so let's tell it to write them to `~/.emacs.d/bak` instead.
;; If you have an accident, check this directory - you might get lucky.
(setq backup-directory-alist
      `(("." . ,(expand-file-name (concat emacs-d "bak")))))

;; Automatically save buffers before launching M-x compile and friends,
;; instead of asking you if you want to save.
(setq compilation-ask-about-save nil)
(auto-save-mode)

;; Make the selection work like most people expect.
(delete-selection-mode t)
(transient-mark-mode t)

;; Automatically update unmodified buffers whose files have changed.
(global-auto-revert-mode 1)

(provide 'etude-base)
