(require 'etude-core-management)

(use-package dired-subtree :ensure t
  :bind (:map dired-mode-map
         ("i" . dired-subtree-toggle)))

(use-package dired-collapse :ensure t
  :hook (dired-mode . dired-collapse-mode))

(use-package dired-filter :ensure t
  :hook (dired-mode . (lambda () (dired-filter-group-mode)
                                 (dired-filter-by-garbage)))
  :custom
    (dired-garbage-files-regexp
      "\\(?:\\.\\(?:aux\\|bak\\|dvi\\|log\\|orig\\|rej\\|toc\\|out\\)\\)\\'")
    (dired-filter-group-saved-groups
     '(("default"
        ("Org"    (extension "org"))
        ("Executables" (exexutable))
        ("Directories" (directory))
        ("PDF"    (extension "pdf"))
        ("LaTeX"  (extension "tex" "bib"))
        ("Images" (extension "png"))
        ("Code"   (extension "java" "el" "clj" "js"))
        ("Archives"(extension "zip" "rar" "gz" "bz2" "tar"))))))

(provide 'etude-core-search)

(use-package beacon :ensure t
  :diminish
  :config (setq beacon-color "#666600")
  :hook   ((org-mode text-mode) . beacon-mode))

;; (e/menu [])
;; 
;; (defhydra e/ivy-occur ()
;;   ("z" e/preview-exit      "cancel" :exit t))
;; 
;; (use-package major-mode-hydra
;;   :ensure t
;;   )
;; 
;; (defvar jp-toggles--title (with-faicon "toggle-on" "Toggles" 1 -0.05))
;; 
;; (pretty-hydra-define jp-toggles
;;   (:color amaranth :quit-key "q" :title "TOGGLES")
;;   ("Basic"
;;    (("n" linum-mode "line number" :toggle t)
;;     ("w" whitespace-mode "whitespace" :toggle t)
;;     ("W" whitespace-cleanup-mode "whitespace cleanup" :toggle t)
;;     ("r" rainbow-mode "rainbow" :toggle t)
;;     ("L" page-break-lines-mode "page break lines" :toggle t))
;;    "Highlight"
;;    (("s" symbol-overlay-mode "symbol" :toggle t)
;;     ("l" hl-line-mode "line" :toggle t)
;;     ("x" highlight-sexp-mode "sexp" :toggle t)
;;     ("t" hl-todo-mode "todo" :toggle t))
;;    "UI"
;;    (("d" jp-themes-toggle-light-dark "dark theme" :toggle jp-current-theme-dark-p))
;;    "Coding"
;;    (("p" smartparens-mode "smartparens" :toggle t)
;;     ("P" smartparens-strict-mode "smartparens strict" :toggle t)
;;     ("S" show-smartparens-mode "show smartparens" :toggle t)
;;     ("f" flycheck-mode "flycheck" :toggle t))
;;    "Emacs"
;;    (("D" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
;;     ("X" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit)))))
;; 
;; (use-package helpful
;;   :ensure t
;;   :pretty-hydra
;;   ((:color teal :quit-key "q")
;;    ("Helpful"
;;     (("f" helpful-callable "callable")
;;      ("v" helpful-variable "variable")
;;      ("k" helpful-key "key")
;;      ("c" helpful-command "command")
;;      ("d" helpful-at-point "thing at point"))))
;; 
;;   :bind ("C-h" . helpful-hydra/body))
;; 
;; 
;; 
;; (e/comment (add-hook 'ivy-occur-mode-hook
;;                     '(lambda ()
;;                        (e/ivy-occur/body))))
;; 
;; (add-hook 'ivy-mode-hook
;;           '(lambda ()
;;              (define-key ivy-minibuffer-map (kbd "C-c C-c") 'ivy-dispatching-done)))
;; 
;; (e/comment
;;     
;;     
;;     (define-key ivy-occur-mode-map "C-c C-c" 'ivy-done)
;;   (define-key ivy-occur-mode-map "<f7>" 'ivy-done)
;; 
;;   (e/mode [::ivy-occur  ivy-occur-mode "etude-module-search"]
;;     ::mode-menu  'e/ivy-occur/body)
;;   
;; 
;; 
;;   (parseedn-print-str ivy-occur-mode-map)
;;   (parseedn-print-str (cons 4 'ivy-occur-mode-map))
;;   (type-of ivy-occur-mode-map)
;;   (type-of (cdr (cadr ivy-occur-mode-map)))
;; 
;;   ivy-occur-mode-map)

