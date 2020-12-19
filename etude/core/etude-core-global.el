(require 'etude-lang)

;;
;; (GLOBAL) Mode Specific
;; 
;; This is reserved for the given language minor mode to provide
;; functionality that is specific working within the language.
;; Usually done for tutorial/walk-through/late-night-brain-dead
;; programming to show common options that a user might be able to use.
;;
;;   - emacs lisp
;;   - java
;;   - clojure
;;   - rust
;;   - c/c++
;;   - verilog
;;   - html/css
;;   - javascript

(e/mode-init []
  ::mode-menu          ("<f8>")  ()
  ::eval-cursor        ("C-e")   ("P")
  ::eval-cursor-alt    ("C-x p" "C-x C-p")   ("P")
  ::eval-file          ("C-x x" "C-x C-x")  ()
  ::mode-connect       ("M-c" "ESC c")      ()
  ::mode-build         ()      ()
  ::mode-jump-to       ("C-x ." "C-x C-.")  ()
  ::mode-toggle-test   ()                   ())

(provide 'etude-core-global)
