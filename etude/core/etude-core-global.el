(require 'eta)

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

(eta-modal-init []
  ::eval-cursor        ("C-e")   ("P")
  ::eval-cursor-alt    ("C-x p" "C-x C-p")   ("P")
  ::eval-file          ("C-x x" "C-x C-x")  ()
  ::f5                 ("<f5>")  ()
  ::f6                 ("<f6>")  ()
  ::f7                 ("<f7>")  ()
  ::f8                 ("<f8>")  ()
  ::f9                 ("<f9>")  ()
  ::f10                ("<f10>")  ()
  ::f11                ("<f11>")  ()
  ::mode-menu          ("<f12>")  ()
  ::mode-connect       ("M-c" "ESC c")      ()
  ::mode-build         ()      ()
  ::mode-jump-to       ("C-x ." "C-x C-.")  ()
  ::mode-toggle-test   ("ESC 5" "M-5")      ()
  ::esc-6              ("ESC 6" "M-6")      ()
  ::esc-7              ("ESC 7" "M-7")      ()
  ::esc-8              ("ESC 8" "M-8")      ()
  ::esc-9              ("ESC 9" "M-9")      ()
  ::esc-0              ("ESC 0" "M-0")      ())

(provide 'etude-core-global)
