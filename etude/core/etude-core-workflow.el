;;
;;  ETUDE CORE WORKFLOW
;;
;;  This package standardizes actions and the etude menu system. All global
;;  keybindings as well as menus are defined. `:keyword`s are used to specify
;;  the action performed. The action is associated with keybindings and a function
;;  definition. Menus are then created from actions. This results in a cleaner
;;  way of configuring the emacs environment.
;;
;;  The menu system is based on a number system, as opposed to a letter abbreviation
;;  system. It follows something like an automated phone directory to assist
;;  the user in picking the right choice. The aim is to simplify movement and order
;;  functionality, not merely to overwhelm the user with options.
;;
;;  - Global                           [quit, menu]
;;  - Editing                          [cut, copy, paste, undo]
;;  - Function Keys        (F1-12)
;;  - Window Management    (M-`)       
;;  - File Management      (M-1)       [open, save, close, quit]
;;  - Navigation           (M-2)       [goto, jump, mark]
;;  - Mode Specific        (M-3)       
;;  - Project Management   (M-4)       [find, replace, bulk find, bulk replace] 
;;  - Code Management      (M-5)       [history, status, commit, push]
;;  - App Management       (M-9)       [eshell]
;;  - Config Management    (M-0)       [jump to config]

(ns: etude-core-workflow
  (:require
   etude-core-base
   etude-core-code
   etude-core-management))

;;
;; (GLOBAL)
;;

(defun on/no-action ()
  (interactive))

(on/bind: []
  ::no-action       ()                  'on/no-action
  ::main-menu       ("C-p")             'counsel-M-x
  ::help-bind-key   ("M-h" "C-c C-h")   'describe-key
  ::quit            ("M-q")             'save-buffers-kill-terminal)

;;
;; (EDITING)
;;


(defun on/just-one-space ()
  "Delete all spaces and tabs around point, leaving one space (or N spaces).
If N is negative, delete newlines as well, leaving -N spaces.
See also `cycle-spacing'."
  (interactive)
  (cycle-spacing -1 nil 'single-shot))

(on/bind: []
  ::cut             ("C-a" "M-a")       'kill-region
  ::trim            ("C-x C-k")         'on/just-one-space
  ::copy            ("C-o" "M-o")       'copy-region-as-kill
  ::cut-context     ("C-k")             'paredit-kill
  ::copy-context    ("C-j")             'on/paredit-copy-as-kill
  ::paste           ("C-v" "M-v")       'yank
  ::paste-menu      ("C-x v")           'counsel-yank-pop
  ::undo            ("C--" "M--")       'undo
  ::undo-menu       ("C-x -")           'undo-tree-visualize
  ::redo            ("C-x C-")          'undo-tree-redo)

;;
;; (M-`) Window Management
;;

(defun on/window-list ()
  (seq-filter (lambda (w) 
                (not (equal (buffer-name (window-buffer w))
		            " *NeoTree*")))
              (window-list)))

(defun on/window-alternate ()
  (seq-find (lambda (w)
              (not (equal w (selected-window))))
            (on/window-list)))

(defun on/window-set-buffer (w buff)
  (save-selected-window
    (select-window w)
    (switch-to-buffer buff)))

(defun on/split-window-toggle ()
  (interactive)
  (let: [cnt  (seq-length (on/window-list))]
    (cond ((equal cnt 1)
	   (progn (split-window-below)
                  (on/window-set-buffer (on/window-alternate)
                                        (other-buffer 1))))
          
          ((not (window-full-height-p))
           (progn (delete-other-windows)
                  (split-window-right)
                  (on/window-set-buffer (on/window-alternate)
                                        (other-buffer 1))))
          (:else
           (delete-other-windows)))))

(on/bind: []
  ::window-close         ("<C-backspace>" "M-DEL")   'delete-window      
  ::window-focus         ("<C-return>" "M-RET")      'delete-other-windows
  ::window-split-left    ()                     nil
  ::window-split-right   ()                     'split-window-right
  ::window-split-top     ()                     nil
  ::window-split-bottom  ()                     'split-window-below
  ::window-split-toggle  ("M-w")                'on/split-window-toggle
  ::window-move-left     ("<C-S-left>" "<M-left>"  "ESC <left>")     'windmove-left
  ::window-move-right    ("<C-S-right>" "<M-right>" "ESC <right>")    'windmove-right
  ::window-move-up       ("<C-S-up>" "<M-up>" "ESC <up>")          'windmove-up
  ::window-move-down     ("<C-S-down>" "<M-down>" "ESC <down>")     'windmove-down
  ::window-balance       ()                     'balance-windows
  ::window-swap          ()                     'ace-swap-window
  ::window-h-plus        ()                     'enlarge-window-horizontally
  ::window-h-minus       ()                     'shrink-window-horizontally
  ::window-v-plus        ()                     'enlarge-window
  ::window-v-minus       ()                     'shrink-window)

(on/menu: [::window-menu  ("M-`")]
  "
  ^Position^                  ^Manage^                    Resize      
  _1_: window focus           _o_: swap                      u 
  _2_: window split h         _w_: toggle                  l + r
  _3_: window split v         _d_: window close              d
  _4_: balance windows
  "
  ("1" ::window-focus "window focus")
  ("2" ::window-split-bottom)
  ("3" ::window-split-right)
  ("4" ::window-balance)
  ("o" ::window-swap "window swap")
  ("w" ::window-split-toggle)
  ("d" ::window-close  "window close")
  ("<left>"  ::window-h-minus)
  ("<up>"    ::window-v-minus)
  ("<down>"  ::window-v-plus)
  ("<right>" ::window-h-plus)
  ("x" ::do-nothing "exit" :exit t))

;;
;; (M-1) File and Buffer Management
;;

(defun on/close-buffer ()
  (interactive)
  (if (buffer-file-name (current-buffer))
      (save-buffer))
  (kill-buffer (current-buffer)))

(defun on/close-all-buffers ()
  (interactive)
  (save-some-buffers)
  (mapc 'kill-buffer (buffer-list)))

(defun on/last-used-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun on/revert-buffer ()
  (interactive)
  (revert-buffer :ignore-auto :noconfirm)
  (message "Buffer reverted"))

(defun on/jump-workflow ()
  (interactive)
  (find-library "etude-core-workflow"))

(on/bind: []
  ::open              ("C-c C-o")         'find-file
  ::open-recent       ("M-r" "C-r" "s-r") 'on/ivy-recentf-file
  ::open-project      ("M-t" "C-t" "s-t") 'projectile-find-file
  ::revert            ("C-x C-r")         'on/revert-buffer
  ::save              ("M-s" "C-s")       'save-buffer
  ::save-as           ("C-x C-s")         'write-file
  ::save-all          ("C-x s")           'save-some-buffers
  ::close             ("M-k")             'on/close-buffer
  ::close-select      ("C-c C-k")         'kill-buffer  
  ::close-all         ("C-c C-x C-k")     'on/close-all-buffers
  ::last-used-buffer  ("C-\\" "M-\\")     'on/last-used-buffer
  ::prev-buffer       ("C-/" "M-/")       'previous-buffer
  ::next-buffer       ("C-=" "M-=")       'next-buffer
  ::jump-buffer       ("M-b" "C-b")       'switch-to-buffer
  ::jump-workflow     ("C-x C-0")         'on/jump-workflow
  ::jump-test         ("C-x C-t")         'projectile-find-test-file
  ::locate-project    ("C-c l")           'on/neotree-projectile-root
  ::toggle-project    ("C-c C-l")         'on/neotree-toggle
  ::locate-file       ("C-l" "M-l")       'on/neotree-projectile-locate
  ::stats             ("M-'")             'count-lines-page)

(on/menu: [::file-menu ("M-1")]
  "
  ^File^            ^Open^                ^Save^            ^Close^
  _1_: save all     _4_: open             _6_: save         _8_: close 
  _2_: close all    _5_: open recent      _7_: save as      _q_: quit emacs
  _3_: open file in project
  "
  ("1" ::save-all     "save all")
  ("2" ::close-all    "close all")
  ("3" ::open-project "open file in project")
  ("4" ::open         nil :exit true)
  ("5" ::open-recent  nil :exit true)
  ("6" ::save)
  ("7" ::save-as)
  ("8" ::close)
  ("q" ::quit         "quit emacs")
  ("x" ::do-nothing   "exit" :exit t))

;;
;; (M-2) Navigation and Movement 
;;

(on/bind: []
  ::goto-line       ()             nil
  ::goto-label      ()             nil
  ::mark-label      ()             nil
  ::page-up         ()             nil
  ::page-down       ()             nil
  ::goto-start      ()             nil
  ::goto-end        ()             nil)

;;
;; (M-3) Language Specific
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

(on/mode-init: []
  ::eval-cursor        ("C-e")   ("P")
  ::eval-file          ("M-e" "C-x e")   ()
  ::init               ("M-c")   ())

(on/menu: [::lang-menu  ("M-3")]
  "
  ^Code^            
  _1_: run          
  _2_: test      
  _3_: connect/compile
  "
  ("1" ::run          "run")
  ("2" ::test         "test")
  ("3" ::init         "connect/compile")
  ("x" ::do-nothing   "exit" :exit t))

;;(on/bind:              'on/lang-menu/body)

;;
;; (M-4) Project Management
;;

(on/bind: []
  ::find                ("C-c s" "C-f")        'swiper
  ::search-backward     ("C-c s")     'isearch-backward
  ::search-forward      ("C-c C-s")   'isearch-forward
  ::replace             ()             nil
  ::find-in-project     ("M-f")        'counsel-ag
  ::replace-in-project  ()             nil
  ::find-in-os          ()             nil
  ::line-count          ()             'count-lines-page) 

(comment: e
  'what-cursor-position)

;;
;; (M-5) Code Management
;;

(on/bind: []
  ::git-status         ()             'magit-status
  ::git-doall          ()             'on/magit-add-commit-push
  ::git-rebase         ()             'magit-rebase
  ::git-commit         ()             'magit-commit
  ::git-push           ()             'magit-push
  ::git-log            ()             'magit-log)

(on/menu: [::git-menu ("M-5")]
  "
  ^Basic^                           ^Advanced^
  _1_: git status
  _2_: git add, commit, push
  _3_: git log
  "
  ("1" ::git-status   "git status")
  ("2" ::git-doall    "git add, commit, push")
  ("3" ::git-log      "git log")
  ("x" ::do-nothing   "exit" :exit t))

;;(on/bind: ::git-menu       ("M-5")             'on/git-menu/body)

;;
;; (M-9) OS Management 
;;

(defun on/jump-to-buffer (bname command &rest args)
  (if (not (equal (buffer-name (current-buffer))
                  bname))
      (funcall 'apply command args)
    (previous-buffer)))

(defun on/jump-to-eshell ()
  (interactive)
  (on/jump-to-buffer "*eshell*" 'eshell))

(defun on/jump-to-start-screen ()
  (interactive)
  (on/jump-to-buffer "*dashboard*" 'on/start-screen))

(defun on/jump-to-scratch ()
  (interactive)
  (on/jump-to-buffer "*scratch*" (lambda () (switch-to-buffer "*scratch*"))))

(on/bind: []
  ::toggle-eshell      ()            'on/jump-to-eshell)

(on/menu: [::os-menu ("M-9")]
  "
  ^OS^                      
  _1_: eshell
  "
  ("1" ::toggle-eshell  "eshell")
  ("x" ::do-nothing   "exit" :exit t))

;;
;; (M-0) Config Managment
;;
;; This is reserved for minor modes to setup their own `jump-to-<LANG>-config' entry
;;

(require 'find-func)

(setq on/*back-buffer* nil)

(defun on/jump-to-config (library)
  (setq on/*back-buffer* (current-buffer))
  (find-library library))

(defun on/jump-back ()
  (interactive)
  (if on/*back-buffer*
      (switch-to-buffer on/*back-buffer*)))


;;
;; (F1 - F12) Function Keys
;;

(on/bind: []
  ::f1              ("<f1>")            'on/jump-to-eshell
  ::f2              ("<f2>")            'on/neotree-toggle
  ::f3              ("<f3>")            'on/jump-to-start-screen
  ::f4              ("<f4>")            'on/jump-to-scratch
  ::f5              ("<f5>")            nil
  ::f6              ("<f6>")            nil
  ::f7              ("<f7>")            nil
  ::f8              ("<f8>")            nil
  ::f9              ("<f9>")            nil
  ::f10             ("<f10>")           nil
  ::f11             ("<f11>")           nil
  ::f12             ("<f12>")           nil)
