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
;;  functionality, not merely to overswhelm the user with options.
;;
;;  - Global                           [quit, menu]
;;  - Editing                          [cut, copy, paste, undo]
;;  - Function Keys        (F1-12)
;;  - Window Management    (F-2)       
;;  - Navigation           (F-3)       [goto, jump, mark]
;;  - File Management      (F-4)       [open, save, close, quit]
;;  - Mode Specific        (M-3)       
;;  - Project Management   (M-4)       [find, replace, bulk find, bulk replace] 
;;  - Code Management      (M-5)       [history, status, commit, push]
;;  - App Management       (M-9)       [eshell]
;;  - Config Management    (M-0)       [jump to config]

(ns: etude-core-workflow
  (:require
   find-func
   etude-core-base
   etude-core-code
   etude-core-management))

;;
;; (GLOBAL)
;;

(setq on/*back-buffer* nil)

(defun on/no-action () (interactive))

(on/bind: []
  ::no-action       ()                          'on/no-action
  ::main-menu       ("C-p" "M-p" "M-x" "ESC p" "ESC x")   'counsel-M-x
  ::quit            ("ESC q" "M-q")             'save-buffers-kill-terminal)

;;
;; (M-8) Language Specific
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
  ::eval-macro         ()        ("P")
  ::eval-file          ("M-e" "ESC e" "C-x e" "C-x C-e")   ()
  ::init               ("M-c" "ESC c")      ()
  ::goto-definition    ("C-x ." "C-x C-.")  ()
  ::mode-menu          ("<f8>")  ()
  ::toggle-test        ()                   ())

;;
;; (EDITING)
;;

(defun on/just-one-space ()
  "Delete all spaces and tabs around point, leaving one space (or N spaces).
If N is negative, delete newlines as well, leaving -N spaces.
See also `cycle-spacing'."
  (interactive)
  (cycle-spacing -1 nil 'single-shot))

(defun on/undo-tree-redo ()
  (interactive)
  (undo-tree-mode t)
  (undo-tree-redo))

(on/bind: []
  ::cut             ("C-a")                       'kill-region
  ::trim            ()                            'on/just-one-space
  ::copy            ("C-o")                       'copy-region-as-kill
  ::cut-context     ("C-k")                       'paredit-kill
  ::copy-context    ("ESC k")                     'on/paredit-copy-as-kill
  ::paste           ("C-v" "M-v" "C-j")           'yank
  ::paste-menu      ("C-x C-v" "C-x v")           'counsel-yank-pop
  ::undo            ("C--")                       'undo
  ::redo            ("ESC -")                     'on/undo-tree-redo
  ::undo-menu       ("C-x -")                     'undo-tree-visualize
  ::comment         ("ESC ;" "C-x ;" "C-x C-;")   'comment-or-uncomment-region)


;;
;; (Buffers and Jumps)
;;


(defun on/last-used-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun on/revert-buffer ()
  (interactive)
  (revert-buffer :ignore-auto :noconfirm)
  (message "Buffer reverted:" ))

(defun on/bufler-toggle ()
  (interactive)
  (if (equal "*Bufler*" (buffer-name (current-buffer)))
      (progn (other-window 1)
             (if (equal "*Bufler*" (buffer-name (current-buffer)))
                 (on/close-buffer)
               (delete-other-windows)))
    (bufler)))

(on/bind: []
  ::jump-buffer       ("ESC b" "M-b" "C-b")   'counsel-switch-buffer
  ::list-buffers      ("C-x b" "C-x C-b")     'on/bufler-toggle
  ::revert-buffer     ("C-x r" "C-x C-r")     'on/revert-buffer
  ::last-used-buffer  ("ESC \\" "M-\\" "C-\\" "C-x \\")   'on/last-used-buffer
  
  ::prev-buffer
  ("M-/" "ESC /" "C-x /" "C-x C-/" "C-S-<left>" "C-x C-<left>")
  'previous-buffer

  ::next-buffer
  ("M-=" "ESC =" "C-x =" "C-x C-=" "C-S-<right>" "C-x C-<right>")
  'next-buffer)

(on/bind: []
  ::goto-line            ("C-x C-l" "C-x l")   'goto-line
  ::goto-start           ()   'beginning-of-buffer
  ::goto-end             ()   'end-of-buffer

  ::jump-search          ("C-l")      'swiper     
  ::jump-bookmark        ()           'counsel-bookmark
  ::jump-search-project  ("C-f")      'counsel-rg) 

;;
;; (F1) Help and References
;;

(defun on/jump-back ()
  (interactive)
  (when on/*back-buffer*
    (switch-to-buffer on/*back-buffer*)
    (setq on/*back-buffer* nil)))

(defun on/jump-to-workflow ()
  (interactive)
  (if on/*back-buffer*
      (on/jump-back)
    (progn (setq on/*back-buffer* (current-buffer))
           (find-library "etude-core-workflow"))))

(on/bind: []
  ::help-for-help       ()                     'help-for-help
  ::about-emacs         ("C-h C-a")            'about-emacs
  ::view-keystroke      ("ESC <f1>" "ESC h" "M-h")      'describe-key
  ::view-function       ()                     'describe-function
  ::view-bindings       ()                     'describe-bindings
  ::view-package        ()                     'describe-package
  ::conf-customise      ()    'customize
  ::conf-workflow       ()    'on/jump-to-workflow)

(on/menu: [::help-menu  ("<f1>")]
  "
  ^Help^                               ^Settings^
   _0_: Help for Help                  _u_: Customise Emacs
   _1_: About Emacs                    _w_: Customise Workflow
   _2_: About GNU            
   _3_: View Distribution Info            
   _4_: View Bindings
   _5_: View Function
   _6_: View Package        
   "
  ("0" ::help-for-help :exit t)
  ("1" ::about-emacs)
  ("2" ::view-distribution)
  ("3" ::view-keystroke)
  ("4" ::view-bindings)
  ("5" ::view-function)
  ("6" ::view-package)
  ("u" ::conf-customise nil :exit t)
  ("w" ::conf-workflow nil :exit t)
  
  ("z" ::do-nothing "cancel" :exit t))



;;
;; (F2) Find Options
;;
;; This is reserved for minor modes to setup their own `jump-to-<LANG>-config' entry
;;


(on/bind: []
  )

(on/menu: [::counsel-menu  ("<f2>")]
  "
  ^Describe^        ^Config^            ^Stats^"
  ("z" ::do-nothing  "cancel" :exit t)
 )



;;
;; (F3) Window Management
;;

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

(defun on/window-delete ()
  (interactive)
  (delete-other-windows)
  (on/close-buffer))

(on/bind: []
  ::window-delete        ("ESC <deletechar>")           'on/window-delete

  ::window-close
  ("ESC DEL" "M-DEL" "C-<backspace>" "C-x DEL" "C-x C-<down>")
  'delete-window      

  ::window-focus
  ("ESC RET" "M-RET" "C-<return>" "C-x RET" "C-x C-<up>")
  'delete-other-windows

  ::window-split-left    ()                     nil
  ::window-split-right   ()                     'split-window-right
  ::window-split-top     ()                     nil
  ::window-split-bottom  ()                     'split-window-below
  ::window-split-toggle  ()                     'on/split-window-toggle
  ::window-move-left     ("<M-left>"  "ESC <left>"   "C-x <left>")     'windmove-left
  ::window-move-right    ("<M-right>" "ESC <right>"  "C-x <right>")    'windmove-right
  ::window-move-up       ("<M-up>"    "ESC <up>" "C-x <up>")          'windmove-up
  ::window-move-down     ("<M-down>"  "ESC <down>" "C-x <down>")     'windmove-down
  ::window-balance       ()                     'balance-windows
  ::window-swap          ()                     'ace-swap-window
  ::window-h-plus        ()                     'enlarge-window-horizontally
  ::window-h-minus       ()                     'shrink-window-horizontally
  ::window-v-plus        ()                     'enlarge-window
  ::window-v-minus       ()                     'shrink-window
  ::window-grow-font     ()                     nil
  ::window-shrink-font   ()                     nil)

(on/menu: [::window-menu  ("<f3>")]
  "
  ^Position^                  ^Manage^                    Resize      
  _1_: window focus           _o_: swap                   _<up>_
  _2_: window split h         _w_: toggle           _<left>_    _<right>_    
  _3_: window split v         _d_: window close          _<down>_
  _4_: balance windows        _9_: shrink font         _0_: grow font
  "
  ("1" ::window-focus "window focus")
  ("2" ::window-split-bottom)
  ("3" ::window-split-right)
  ("4" ::window-balance)
  ("9" ::window-shrink-font)
  ("0" ::window-grow-font)
  ("o" ::window-swap "window swap")
  ("w" ::window-split-toggle)
  ("d" ::window-close  "window close")
  ("<left>"  ::window-h-minus)
  ("<up>"    ::window-v-minus)
  ("<down>"  ::window-v-plus)
  ("<right>" ::window-h-plus)
  ("z" ::do-nothing "cancel" :exit t))


;;;
;; (F4) System and Files
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

(on/bind: []
  ::open              ("C-x C-f" "C-x f")          'find-file
  ::open-recent       ("C-r")                      'counsel-recentf
  ::open-project      ("ESC t" "M-t" "C-t" "s-t")  'projectile-find-file-dwim
  
  ::save              ("ESC s" "C-s" "M-s")        'save-buffer
  ::save-as           ()                           'write-file
  ::save-all          ()                           'save-some-buffers
  ::close             ("ESC `" "M-`" "C-`")        'on/close-buffer
  ::close-select      ()    'kill-buffer  
  ::close-all         ()    'on/close-all-buffers
  ::find              ()    'on/menu-fn::find-menu/body
  ::help              ()    'on/menu-fn::help-menu/body
  ::run-vterm         ()    'multi-vterm
  ::directory         ("ESC 0" "M-0" "C-x 0" "C-x C-0" "C-0")   'treemacs)

(on/menu: [::file-menu ("<f4>")]
  "
  ^File^                 ^Save^               ^Preferences^
  _1_: open/new          _6_: find            _h_: help
  _2_: open recent       _7_: save            
  _3_: open project      _8_: save as         
  _4_: close             _9_: save all        
  _5_: close all         _0_: directory       _v_: vterm
  "
  ("1" ::open         nil :exit t)
  ("2" ::open-recent  nil :exit t)
  ("3" ::open-project nil :exit t)
  ("4" ::close)
  ("5" ::close-all)
  ("6" ::find nil)
  ("7" ::save)
  ("8" ::save-as)
  ("9" ::save-all)
  ("0" ::directory nil :color blue)
  ("h" ::help nil :exit t)
  ("v" ::run-vterm nil :exit t)
  ("q" ::quit         "quit emacs" :exit t)
  ("z" ::do-nothing   "cancel" :exit t))


;;
;; (F5) Applications
;;
;; This is reserved for minor modes to setup their own `jump-to-<LANG>-config' entry
;;


(on/bind: []
  )

(on/menu: [::counsel-menu  ("<f2>")]
  "
  ^Describe^        ^Config^            ^Stats^"
  ("z" ::do-nothing  "cancel" :exit t)
 )



;;
;; (M-6) Container Management
;;




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
  ::toggle-eshell          ("ESC 1" "M-1")                      'on/jump-to-eshell
  ::toggle-dashboard       ("ESC 2" "M-2")                      'on/jump-to-start-screen
  ::toggle-scratch         ("ESC 3" "M-3")                      'on/jump-to-scratch
  ::toggle-magit           ("ESC 4" "M-4")                      'magit
  ::toggle-workflow        ("ESC 5" "M-5")                      'on/jump-to-workflow
  ::toggle-treemacs        ("ESC 0" "M-0")                      'treemacs-find-file)

(on/menu: [::os-menu ("<f9>")]
  "
  ^OS^                      
  _1_: toggle eshell         _4_: toggle git
  _2_: toggle dashboard      _5_: toggle workflow
  _3_: toggle scratch        
  "
  ("1" ::toggle-eshell)
  ("2" ::toggle-dashboard)
  ("3" ::toggle-scratch)
  ("4" ::toggle-magit)
  ("5" ::toggle-workflow)
  ("z" ::do-nothing   "cancel" :exit t))

