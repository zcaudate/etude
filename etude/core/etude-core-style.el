(ns: etude-core-style)

;; Remove menu, tool, and scrolls
(set-cursor-color "light green")
(show-paren-mode true)
(global-hl-line-mode true)
(setq redisplay-dont-pause true)

;; When not in a terminal, configure a few window system specific things.
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode false)
  (mouse-wheel-mode true)
  (blink-cursor-mode false))

(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode false)))

(use-package solarized-theme
  :ensure t
  :init (load-theme 'solarized-dark true))

;; Use smart mode line
(use-package smart-mode-line
  :ensure t
  :init   (setq sml/no-confirm-load-theme true)
  :config (progn (sml/setup)
                 (sml/apply-theme 'smart-mode-line-dark)))

(progn (global-display-line-numbers-mode true)
       (setq display-line-numbers "%4d \u2502 ")
       
       ;; Show column numbers in modeline.
       (setq column-number-mode true)

       ;; Show current function in modeline.
       (which-function-mode true))

(use-package auto-highlight-symbol
  :ensure t
  :diminish 'auto-highlight-symbol-mode
  :config   (progn (setq ahs-idle-interval 0.0)
	                 (global-auto-highlight-symbol-mode true)))
 
(provide 'etude-core-style)
