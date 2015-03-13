;; Use solarized theme
(use-package solarized-theme :ensure t
  :init (load-theme 'solarized-dark t))

;; Remove menu, tool, and scrolls
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Use smart mode line
(use-package smart-mode-line
  :ensure t
  :init (setq sml/no-confirm-load-theme t)
  :config 
  (progn
    (sml/setup)
    (sml/apply-theme 'smart-mode-line-dark)))

;; Configure the dark colour scheme.
(linum-mode)
(set-face-attribute 'linum nil :foreground "steel blue" :background nil :height 0.7)

;; Don't defer screen updates when performing operations.
(setq redisplay-dont-pause t)

;; When not in a terminal, configure a few window system specific things.
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

;; Show line numbers in buffers.
(global-linum-mode t)
(setq linum-format (if (not window-system) "%4d " "%4d"))

;; Show column numbers in modeline.
(setq column-number-mode t)

;; Show current function in modeline.
(which-function-mode)

;; Ensure linum-mode is disabled in certain major modes.
(setq linum-disabled-modes
      '(term-mode slime-repl-mode magit-status-mode help-mode nrepl-mode
        mu4e-main-mode mu4e-headers-mode mu4e-view-mode
        mu4e-compose-mode))

(defun linum-on ()
  (unless (or (minibufferp) (member major-mode linum-disabled-modes))
    (linum-mode 1)))

;; Highlight matching braces.
(show-paren-mode 1)


(provide 'etude-style)