(require 'etude-core-global)

;; Lisp Modes
(use-package smartparens
  :ensure t
  :diminish 'smartparens-mode
  :config (progn (require 'smartparens-config)
                 (smartparens-global-mode t)
                 (show-paren-mode t)))

(use-package paredit
  :ensure t
  :diminish 'paredit-mode)

(use-package rainbow-delimiters 
  :ensure t
  :diminish 'rainbow-delimiters-mode)

(progn (add-hook 'lisp-interaction-mode-hook 'smartparens-strict-mode)
       (add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
       (add-hook 'lisp-interaction-mode-hook 'paredit-mode)
       (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
       (add-hook 'lisp-interaction-mode-hook 'annotate-mode)
       (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
       (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
       (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
       (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
       (add-hook 'lisp-interaction-mode-hook 'annotate-mode))

(defun e/eval-buffer ()
  (interactive)
  (eval-buffer (current-buffer) t))

(pretty-hydra-define e/menu-fn::elisp-menu
  (:title "<F8> Elisp" :quit-key "z"  :exit nil :foreign-keys run)
  ("Actions"
   (("1" ielm "Repl")
    ("2" ert  "Test")
    ("3" (ert t) "Test all")
    ("4" (ert :failed) "Test failed"))
   "Describe"
   (("f"  helpful-callable   "function")
    ("s"  helpful-symbol     "symbol")
    ("c"  helpful-command    "command")
    ("i"  helpful-at-point   "thing at point"))
   "Toggle"
   (("P" paredit-mode "Paredit" :toggle t)
    ("R" rainbow-delimiters-mode "Rainbow" :toggle t)
    ("D" toggle-debug-on-error "Debug on error" :toggle (default-value 'debug-on-error))
    ("X" toggle-debug-on-quit "Debug on quit" :toggle (default-value 'debug-on-quit)))))

(e/mode [::lisp   lisp-interaction-mode "etude-core-global"]
  ::eval-cursor   'eval-last-sexp
  ::eval-file     'e/eval-buffer
  ::eval-cursor-alt 'pp-macroexpand-last-sexp
  ::mode-menu     'e/menu-fn::elisp-menu/body)

(e/mode [::emacs-lisp    emacs-lisp-mode "etude-core-lisp"]
  ::eval-cursor     'eval-last-sexp
  ::eval-file       'e/eval-buffer
  ::eval-cursor-alt 'pp-macroexpand-last-sexp
  ::mode-menu       'e/menu-fn::elisp-menu/body)



;; (e/mode [::eshell-mode   eshell-mode    "etude-core-lisp"]
;;   ::eval-cursor 'eval-last-sexp)

;;
;; Clojure
;;


(use-package cider
  :ensure t
  :init (progn (setq nrepl-log-messages t)
               (setq nrepl-buffer-name-separator "/")
               (setq nrepl-buffer-name-show-port t)
               (setq cider-prefer-local-resources t)
               (setq cider-repl-use-clojure-font-lock t))
  :hook ((cider-repl-mode . smartparens-strict-mode)
         (cider-repl-mode . rainbow-delimiters-mode)
         (cider-repl-mode . eldoc-mode)))

(defun e/cider-eval-buffer ()
  (interactive)
  (save-buffer)
  (cider-eval-buffer))

(use-package clojure-mode
  :ensure t
  :config (progn (require 'cider-mode)
                 (require 'midje-mode)
                 (e/mode [::clojure clojure-mode "etude-module-jvm"]
                   ::eval-cursor   'cider-eval-last-sexp
                   ::eval-file     'e/cider-eval-buffer
                   ::init          'cider-connect))
  :hook ((clojure-mode . smartparens-strict-mode)
         (clojure-mode . rainbow-delimiters-mode)
         (clojure-mode . paredit-mode)
         (clojure-mode . eldoc-mode)
         (clojure-mode . annotate-mode)))

(use-package midje-mode
  :ensure t
  :config (define-clojure-indent
            (comment 'defun)))

;;
;; Overlays from cider
;;

(autoload 'cider--make-result-overlay "cider-overlays")

(defun endless/eval-overlay (value point)
  (cider--make-result-overlay (format "%S" value)
    :where point
    :duration 'command)
  ;; Preserve the return value.
  value)

(advice-add 'eval-region :around
            (lambda (f beg end &rest r)
              (endless/eval-overlay
               (apply f beg end r)
               end)))

(advice-add 'eval-last-sexp :filter-return
            (lambda (r)
              (endless/eval-overlay r (point))))

(advice-add 'eval-defun :filter-return
            (lambda (r)
              (endless/eval-overlay
               r
               (save-excursion
                 (end-of-defun)
                 (point)))))

(provide 'etude-core-lisp)


;; (major-mode-hydra-define emacs-lisp-mode nil
;;   ("Eval"
;;    (("b" eval-buffer "buffer")
;;     ("e" eval-defun "defun")
;;     ("r" eval-region "region"))
;;    "REPL"
;;    (("I" ielm "ielm"))
;;    "Test"
;;    (("t" ert "prompt")
;;     ("T" (ert t) "all")
;;     ("F" (ert :failed) "failed"))
;;    "Doc"
;;    (("d" describe-foo-at-point "thing-at-pt")
;;     ("f" describe-function "function")
;;     ("v" describe-variable "variable")
;;     ("i" info-lookup-symbol "info lookup"))));
