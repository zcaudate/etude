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
  ::eval-cursor-alt    ("C-c e" "C-x p" "C-x C-p")   ("P")
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
  ::cc-tilda           ("C-c `")      ()
  ::cc-1               ("C-c 1")      ("P")
  ::cc-2               ("C-c 2")      ("P")
  ::cc-3               ("C-c 3")      ("P")
  ::cc-4               ("C-c 4")      ("P")
  ::cc-5               ("C-c 5")      ("P")
  ::cc-6               ("C-c 6")      ("P")
  ::cc-7               ("C-c 7")      ("P")
  ::cc-8               ("C-c 8")      ("P")
  ::cc-9               ("C-c 9")      ("P")               
  ::cc-0               ("C-c 0")      ("P")
  ::mode-toggle-test   ("ESC 5" "M-5")      ()
  ::esc-6              ("ESC 6" "M-6")      ()
  ::esc-7              ("ESC 7" "M-7")      ()
  ::esc-8              ("ESC 8" "M-8")      ()
  ::esc-9              ("ESC 9" "M-9")      ()
  ::esc-0              ("ESC 0" "M-0")      ()
  ::esc-f2             ("ESC <f2>")  ()
  ::esc-f3             ("ESC <f3>")  ()
  ::esc-f4             ("ESC <f4>")  ()
  ::esc-f5             ("ESC <f5>")  ()
  ::esc-f6             ("ESC <f6>")  ()
  ::esc-f7             ("ESC <f7>")  ()
  ::esc-f8             ("ESC <f8>")  ()
  ::esc-f9             ("ESC <f9>")  ()
  ::esc-f10            ("ESC <f10>")  ()
  ::esc-f11            ("ESC <f11>")  ())

(provide 'etude-core-global)
