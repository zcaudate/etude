;;; ETUDE GLOBAL ACTION
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

(require 'etude-global-base)
(require 'etude-global-management)
(require 'etude-global-code)

;;
;; (GLOBAL)
;;

(bind:
 ::no-action       ()                  'etude/no-action
 ::main-menu       ("C-p" "M-x")       'counsel-M-x
 ::help-bind-key   ("M-h")             'describe-key
 ::quit            ("M-q")             'save-buffers-kill-terminal)

;;
;; (EDITING)
;;

(bind:
 ::cut             ("C-a")             'kill-region    
 ::copy            ("C-j")             'copy-region-as-kill
 ::cut-context     ("C-k")             'paredit-kill
 ::copy-context    ("C-x C-k")         'etude/paredit-copy-as-kill
 ::paste           ("C-o" "C-v")       'yank
 ::paste-menu      ("M-v" "C-x C-v")   'counsel-yank-pop
 ::undo            ("C--")             'undo
 ::undo-menu       ("M-_" "C-x C--")   'undo-tree-visualize
 ::redo            ("M--")             'undo-tree-redo)

;;
;; (F1 - F12) Function Keys
;;

(bind:
 ::f1              ("<f1>")            'etude/jump-to-eshell
 ::f2              ("<f2>")            'etude/neotree-toggle
 ::f3              ("<f3>")            'etude/jump-to-start-screen
 ::f4              ("<f4>")            'etude/jump-to-scratch
 ::f5              ("<f5>")            nil
 ::f6              ("<f6>")            nil
 ::f7              ("<f7>")            nil
 ::f8              ("<f8>")            nil
 ::f9              ("<f9>")            nil
 ::f10             ("<f10>")           nil
 ::f11             ("<f11>")           nil
 ::f12             ("<f12>")           nil)

;;
;; (M-`) Window Management
;;

(defun etude/split-window-toggle ()
  (interactive)
  (let ((cnt (seq-count
	      (lambda (w)
		(not (equal (buffer-name (window-buffer w))
			    " *NeoTree*")))
	      (window-list))))
    (if (equal cnt 1)
	(split-window-below)
      (delete-other-windows))))

(bind:
 ::window-close         ("M-DEL")              'delete-window      
 ::window-focus         ("M-RET")              'delete-other-windows
 ::window-split-left    ()                     nil
 ::window-split-right   ()                     'split-window-right
 ::window-split-top     ()                     nil
 ::window-split-bottom  ()                     'split-window-below
 ::window-split-toggle  ()                     'etude/split-window-toggle
 ::window-move-left     ("ESC <left>")         'windmove-left
 ::window-move-right    ("ESC <right>")        'windmove-right
 ::window-move-up       ("ESC <up>")           'windmove-up
 ::window-move-down     ("ESC <down>")         'windmove-down
 ::window-balance       ()                     'balance-windows
 ::window-swap          ()                     'ace-swap-window
 ::window-h-plus        ()                     'enlarge-window-horizontally
 ::window-h-minus       ()                     'shrink-window-horizontally
 ::window-v-plus        ()                     'enlarge-window
 ::window-v-minus       ()                     'shrink-window)

(menu:
 ["Window" etude/window-menu
  "
  ^Position^                  ^Manage^                    Resize      
  _1_: window focus           _o_: swap                      u 
  _2_: window split h         _t_: toggle                  l + r
  _3_: window split v         _d_: window close              d
  _4_: balance windows
  "]
 ("1" ::window-focus "window focus")
 ("2" ::window-split-bottom)
 ("3" ::window-split-right)
 ("4" ::window-balance)
 ("o" ::window-swap "window swap")
 ("t" ::window-split-toggle)
 ("d" ::window-close  "window close")
 ("<left>"  ::window-h-minus)
 ("<up>"    ::window-v-minus)
 ("<down>"  ::window-v-plus)
 ("<right>" ::window-h-plus)
 ("x" ::do-nothing "exit" :exit t))

(bind: ::window-menu   ("M-`")                'etude/window-menu/body)

;;
;; (M-1) File and Buffer Management
;;

(defun etude/close-buffer ()
  (interactive)
  (if (buffer-file-name (current-buffer))
      (save-buffer))
  (kill-buffer (current-buffer)))

(defun etude/close-all-buffers ()
  (interactive)
  (save-some-buffers)
  (mapc 'kill-buffer (buffer-list)))

(bind:
 ::open            ("C-o")             'find-file
 ::open-recent     ("C-r")             'etude/ivy-recentf-file
 ::open-project    ("C-t")             'projectile-find-file
 ::save            ("C-s")             'save-buffer
 ::save-as         ("M-s" "C-x C-s")   'write-file
 ::save-all        ("M-S")             'save-some-buffers
 ::close           ("M-k")             'etude/close-buffer
 ::close-all       ("M-K")             'etude/close-all-buffers
 ::prev-buffer     ("M-/")             'previous-buffer
 ::next-buffer     ("M-=")             'next-buffer
 ::jump-buffer     ("C-b")             'switch-to-buffer
 ::toggle-project  ("M-L")             'etude/neotree-toggle
 ::locate-project  ("C-l")             'etude/neotree-projectile-root
 ::locate-file     ("M-l" "C-x C-l")   'etude/neotree-projectile-locate)

(menu:
 ["File" etude/file-menu
  "
  ^Bulk^            ^Open^                ^Save^            ^Close^
  _1_: save all     _4_: open             _6_: save         _8_: close 
  _2_: close all    _5_: open recent      _7_: save as      _q_: quit emacs
  _3_: open file in project
  "]
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

(bind: ::file-menu       ("M-1")             'etude/file-menu/body)

;;
;; (M-2) Navigation and Movement 
;;

(bind:
 ::goto-line       ()             nil
 ::goto-label      ()             nil
 ::mark-label      ()             nil
 ::page-up         ()             nil
 ::page-down       ()             nil
 ::goto-start      ()             nil
 ::goto-end        ()             nil)

;;
;; (M-3) Mode Specific
;; 
;; This is reserved for the given minor mode to provide functionality
;; that is specific working within the language. Usually done for
;; tutorial/walk-through/late-night-brain-dead programming to show
;; common options that a user might be able to use.
;;
;;   - emacs lisp
;;   - java
;;   - clojure
;;   - rust
;;   - c/c++
;;   - verilog
;;   - html/css
;;   - javascript


;;
;; (M-4) Project Management
;;

(bind:
 ::find                ()             nil
 ::replace             ()             nil
 ::find-in-project     ()             nil
 ::replace-in-project  ()             nil
 ::find-in-os          ()             nil) 

;;
;; (M-5) Code Management
;;

(bind:
 ::git-status         ()             'magit-status
 ::git-doall          ()             'etude/magit-add-commit-push
 ::git-rebase         ()             'magit-rebase
 ::git-commit         ()             'magit-commit
 ::git-push           ()             'magit-push
 ::git-log            ()             'magit-log)

(menu:
 ["Git" etude/git-menu
  "
  ^Basic^                           ^Advanced^
  _1_: git status
  _2_: git add, commit, push
  _3_: git log
  "]
 ("1" ::git-status   "git status")
 ("2" ::git-doall    "git add, commit, push")
 ("3" ::git-log      "git log")
 ("x" ::do-nothing   "exit" :exit t))

(bind: ::git-menu       ("M-5")             'etude/git-menu/body)



;;
;; (M-9) OS Management 
;;
(bind:
 ::toggle-eshell      ()            'etude/jump-to-eshell)


;;
;; (M-0) Config Managment
;;
;; This is reserved for minor modes to setup their own `jump-to-<LANG>-config' entry
;;

(require 'find-func)

(setq etude/*back-buffer* nil)

(defun etude/jump-to-config (library)
  (setq etude/*back-buffer* (current-buffer))
  (find-library library))

(defun etude/jump-back ()
  (interactive)
  (if etude/*back-buffer*
      (switch-to-buffer etude/*back-buffer*)))

(bind: emacs-lisp-mode-map
       ::jump-back          ("M-0")          'etude/jump-back)

(provide 'etude-global-action)
