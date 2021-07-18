(require 'etude-core-global)
(require 'etude-foundation)

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

(use-package eros
  :ensure t)

(progn (add-hook 'lisp-interaction-mode-hook 'smartparens-strict-mode)
       (add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
       (add-hook 'lisp-interaction-mode-hook 'paredit-mode)
       (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
       (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
       (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
       (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
       (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
       (add-hook 'emacs-lisp-mode-hook (lambda ()
                                         (if which-key-mode
                                             (which-key-mode t)))))
(defun e/eval-buffer ()
  (interactive)
  (eval-buffer (current-buffer) t))

(pretty-hydra-define e/menu-fn::elisp-menu
  (:title "<F10> Elisp" :quit-key "z"  :exit nil :foreign-keys run)
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

(defun e/elisp-mode-menu ()
  (interactive)
  (if (eq hydra-curr-map e/menu-fn::elisp-menu/keymap)
      (hydra-keyboard-quit)
    (e/menu-fn::elisp-menu/body)))

(eta-modal [::lisp   lisp-interaction-mode "etude-core-global"]
  ::eval-cursor      'eros-eval-last-sexp
  ::eval-cursor-alt  'pp-macroexpand-last-sexp
  ::eval-file        'e/eval-buffer
  ::refresh-code     'e/eval-buffer
  ::refresh-rt       'e/no-action
  ::mode-menu        'e/elisp-mode-menu)

(eta-modal [::emacs-lisp    emacs-lisp-mode "etude-core-lisp"]
  ::eval-cursor     'eros-eval-last-sexp
  ::eval-cursor-alt 'pp-macroexpand-last-sexp
  ::eval-file       'e/eval-buffer
  ::refresh-code    'e/eval-buffer
  ::refresh-rt      'e/no-action
  ::mode-menu       'e/elisp-mode-menu)

;; (eta-modal[::eshell-mode   eshell-mode    "etude-core-lisp"]
;;   ::eval-cursor 'eval-last-sexp)

;;
;; Clojure
;;

;; TEMPLATE
(comment
 (pretty-hydra-define e/menu-fn::clojure-menu 
   (:quit-key "z"  :exit nil :foreign-keys run)
   (""
    (("` 1"  foundation/ptr-print  "ptr-print")
     ("` 2"  foundation/ptr-clip   "ptr-clip"))
    ""
    (("` 3"  cider-eval-last-sexp  "eval"))
    ""
    (("` 5"  foundation/ptr-teardown "ptr-teardown")
     ("` 6"  foundation/ptr-setup     "ptr-setup"))
    ""
    (("3"  foundation/rt-reprep   "Prep Rt"))
    ""
    (("5" foundation/rt-setup     "rt-setup")
     ("6"  foundation/rt-teardown "rt-teardown"))
    
    ""
    (("0"  foundation/import-tests  "Import Tests")
     ("9"  foundation/create-tests  "Create Tests")))))


;; ACTUAL
(defhydra e/menu-fn::clojure-menu (:color pink
                                   :hint nil)
  "
`1: ptr-print   `3: eval   `5: ptr-teardown   3: Prep Rt   5: rt-setup      0: Import Tests 
`2: ptr-clip               `6: ptr-setup                   6: rt-teardown   9: Create Tests    "
  ("` 1"  foundation/ptr-print)
  ("` 2"  foundation/ptr-clip)
  ("` 3"  cider-eval-last-sexp)
  ("` 5"  foundation/ptr-teardown)
  ("` 6"  foundation/ptr-setup)
  ("3"  foundation/rt-reprep)
  ("5" foundation/rt-setup)
  ("6"  foundation/rt-teardown)
  ("0"  foundation/import-tests)
  ("9"  foundation/create-tests)
  ("z" quit-window " " :exit t :hint nil))

(defun e/clojure-mode-menu ()
  (interactive)
  (if (eq hydra-curr-map e/menu-fn::clojure-menu/keymap)
      (hydra-keyboard-quit)
    (e/menu-fn::clojure-menu/body)))

(use-package cider
  :defer t
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
  :defer t
  :config (progn (require 'cider-mode)
                 (require 'midje-mode)
                 (eta-modal [::clojure clojure-mode "etude-core-lisp"]
                   ::eval-cursor       'cider-eval-last-sexp
                   ::eval-file         'e/cider-eval-buffer
                   ::f5                'foundation/ns-reeval
                   ::f6                'foundation/rt-refresh
                   ::f7                'foundation/rt-setup
                   ::f8                'foundation/rt-resetup
                   ::esc-6             'foundation/ptr-print
                   ::mode-connect      'cider-connect
                   ::mode-toggle-test  'projectile-toggle-between-implementation-and-test
                   ::mode-menu         'e/clojure-mode-menu)
                 (eta-modal [::cider-repl cider-repl-mode "etude-core-lisp"]
                   ::eval-file   'cider-repl-clear-buffer))
  :hook ((clojure-mode . smartparens-strict-mode)
         (clojure-mode . rainbow-delimiters-mode)
         (clojure-mode . paredit-mode)
         (clojure-mode . eldoc-mode)))

(use-package midje-mode
  :defer t
  :config (define-clojure-indent
            (comment 'defun)))

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
