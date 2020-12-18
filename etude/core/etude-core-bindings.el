(require 'etude-lang)
(require 'etude-core-code)
(require 'etude-core-git)
(require 'etude-core-global)
(require 'etude-core-lisp)
(require 'etude-core-management)
(require 'etude-core-shell)
(require 'etude-core-util)

;;
;; (Command)
;;

(setq e/*back-buffer* nil)

(defun e/no-action () (interactive))

(e/bind []
  ::no-action       ()                          'e/no-action
  ::main-menu       ("C-p" "M-p" "M-x" "ESC p" "ESC x")   'counsel-M-x
  ::quit            ("ESC q" "M-q")             'save-buffers-kill-terminal
  ::help-key        ("ESC <f1>")                'helpful-key)

(defun e/undo-tree-redo ()
  (interactive)
  (undo-tree-mode t)
  (undo-tree-redo))

(defun e/paredit-copy-context ()
  (interactive)
  (paredit-kill)
  (save-excursion (yank)))

(defun e/paredit-duplicate-context ()
  (interactive)
  (save-excursion
    (paredit-kill)
    (yank)
    (insert "\n")
    (yank)))

(e/bind []
  ::select-all      ("ESC a" "C-x C-a" "C-x a")   'mark-whole-buffer
  ::cut             ("C-a")                       'kill-region
  ::copy            ("C-o")                       'copy-region-as-kill
  ::cut-context     ("C-k")                       'paredit-kill
  ::copy-context    ("C-x k" "C-x C-k")           'e/paredit-copy-context
  ::paste-context   ("C-x C-o" "C-x o")           'e/paredit-duplicate-context
  ::paste           ("C-v" "M-v" "C-j")           'yank
  ::paste-menu      ("C-x C-v" "C-x v")           'counsel-yank-pop
  ::undo            ("C--")                       'undo
  ::redo            ("ESC -")                     'e/undo-tree-redo
  ::undo-menu       ("C-x -" "C-x C--")           'undo-tree-visualize
  ::comment         ("ESC ;" "C-x ;" "C-x C-;")   'comment-or-uncomment-region)
  

;;
;; (Buffers and Jumps)
;;


(defun e/last-used-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun e/revert-buffer ()
  (interactive)
  (revert-buffer :ignore-auto :noconfirm)
  (message "Buffer reverted:" ))

(defun e/bufler-toggle ()
  (interactive)
  (if (equal "*Bufler*" (buffer-name (current-buffer)))
      (progn (other-window 1)
             (if (equal "*Bufler*" (buffer-name (current-buffer)))
                 (e/close-buffer)
               (delete-other-windows)))
    (bufler)))

(e/bind []
  ::jump-buffer       ("ESC b" "M-b" "C-b")   'counsel-switch-buffer
  ::list-buffers      ("C-x b" "C-x C-b")     'e/bufler-toggle
  ::revert-buffer     ("C-x r" "C-x C-r")     'e/revert-buffer
  ::last-used-buffer  ("ESC \\" "M-\\" "C-\\" "C-x \\")   'e/last-used-buffer

  ::prev-buffer
  ("M-/" "ESC /" "C-x /" "C-x C-/" "C-S-<left>" "C-x C-<left>" "C-x h" "C-x C-h")
  'previous-buffer

  ::next-buffer
  ("M-=" "ESC =" "C-x =" "C-x C-=" "C-S-<right>" "C-x C-<right>" "C-x l" "C-x C-l")
  'next-buffer)

(e/bind []
  ::goto-line            ("C-x C-g" "C-x g")   'goto-line
  ::goto-start           ()   'beginning-of-buffer
  ::goto-end             ()   'end-of-buffer

  ::jump-search          ("C-l")      'swiper     
  ::jump-bookmark        ()           'counsel-bookmark
  ::jump-search-project  ("C-f")      'counsel-rg) 

;;
;; (Window Movement)
;;


(defun e/window-delete ()
  (interactive)
  (delete-other-windows)
  (e/close-buffer))

(e/bind []
  ::window-delete
  ("ESC <deletechar>" "C-x 9" "C-x C-d" "C-x d")
  'e/window-delete
  
  ::window-close
  ("ESC DEL" "M-DEL" "C-<backspace>" "C-x DEL" "C-x C-<down>")
  'delete-window      

  ::window-focus
  ("ESC RET" "M-RET" "C-<return>" "C-x RET" "C-x C-<up>" "C-x t" "C-x C-t")
  'delete-other-windows
  
  ::window-move-left     ("<M-left>"  "ESC <left>"   "C-x <left>")     'windmove-left
  ::window-move-right    ("<M-right>" "ESC <right>"  "C-x <right>")    'windmove-right
  ::window-move-up       ("<M-up>"    "ESC <up>" "C-x <up>")          'windmove-up
  ::window-move-down     ("<M-down>"  "ESC <down>" "C-x <down>")     'windmove-down)



;;
;; (F1) Help
;;


(pretty-hydra-define e/menu-fn::help-menu
  (:title "<F1> Help" :quit-key "z")
  ("Emacs"
   (("0" about-emacs "about")
    ("1" describe-distribution "distribution")
    ("2" describe-mode      "current mode"))
   "Packages"
   (("l" package-list-packages  "list")
    ("R" package-refresh-contents "refresh")
    ("i" package-install "install")
    ("d" package-remove "delete"))
   "Reference"
   (("b" describe-bindings  "list bindings"))))

(e/bind [] ::f1-menu   ("<f1>")   'e/menu-fn::help-menu/body)

;;
;; (F2) Applications
;;

(defun e/jump-back ()
  (interactive)
  (when e/*back-buffer*
    (switch-to-buffer e/*back-buffer*)
    (setq e/*back-buffer* nil)))

(defun e/jump-to-bindings ()
  (interactive)
  (if e/*back-buffer*
      (e/jump-back)
    (progn (setq e/*back-buffer* (current-buffer))
           (find-library "etude-core-bindings"))))

(defun e/jump-to-buffer (bname command &rest args)
  (if (not (equal (buffer-name (current-buffer))
                  bname))
      (funcall 'apply command args)
    (previous-buffer)))

(defun e/jump-to-terminal ()
  (interactive)
  (e/jump-to-buffer "vterm" 'vterm))

(defun e/jump-to-start-screen ()
  (interactive)
  (e/jump-to-buffer "*dashboard*" 'e/start-screen))

(defun e/jump-to-scratch ()
  (interactive)
  (e/jump-to-buffer "*scratch*" (lambda () (switch-to-buffer "*scratch*"))))

(e/bind []
  ::toggle-terminal        ("ESC 1" "M-1")   'e/jump-to-terminal
  ::toggle-dashboard       ("ESC 2" "M-2")   'e/jump-to-start-screen
  ::toggle-scratch         ("ESC 3" "M-3")   'e/jump-to-scratch
  ::toggle-magit           ("ESC 4" "M-4")   'magit
  ::toggle-bindings        ("ESC 5" "M-5")   'e/jump-to-bindings
  ::toggle-kmacro          ("ESC 6" "M-6")   'e/menu-fn::kmacro-menu/body)

(pretty-hydra-define e/menu-fn::app-menu
  (:title "<F2> Applications" :quit-key "z")
  ("Jump To"
   (("1" e/jump-to-terminal "terminal")
    ("2" e/jump-to-start-screen "dashboard")
    ("3" e/jump-to-scratch      "scratch")
    ("4" magit      "git")
    ("5" e/jump-to-bindings      "configuration")
    ("6" e/menu-fn::kmacro-menu/body  "keyboard macros" :exit t))))

(e/bind [] ::f2-menu   ("<f2>")   'e/menu-fn::app-menu/body)

(e/bind [] ::f4-menu   ("<f4>")   'e/menu-fn::file-menu/body)
;;
;; (F3) Window Movement
;;


(defun e/window-alternate ()
  (seq-find (lambda (w)
              (not (equal w (selected-window))))
            (window-list)))

(defun e/window-set-buffer (w buff)
  (save-selected-window
    (select-window w)
    (switch-to-buffer buff)))

(defun e/split-window-toggle ()
  (interactive)
  (e/let [cnt  (seq-length (window-list))]
    (cond ((equal cnt 1)
	   (progn (split-window-below)
                  (e/window-set-buffer (e/window-alternate)
                                        (other-buffer 1))))
        
          ((not (window-full-height-p))
           (progn (delete-other-windows)
                  (split-window-right)
                  (e/window-set-buffer (e/window-alternate)
                                        (other-buffer 1))))
          (:else
           (delete-other-windows)))))

(pretty-hydra-define e/menu-fn::window-menu
  (:color red :quit-key "z" :title "<F3> Window")
  ("Current"
   (("0"   split-window-right   "split h")
    ("1"   split-window-below   "split v")
    ("DEL" delete-window   "close")
    ("RET" delete-other-windows "focus"))

   "Manage"
   (("b"   'balance-windows-area "balance")
    ("d"   e/window-delete "delete")
    ("o"   e/split-window-toggle "toggle")
    ("w"   ace-swap-window "swap"))
   
   ""
   (("<left>"  shrink-window-horizontally ""))
   "Resize"
   (("<up>"    shrink-window  "")
    ("<down>"  enlarge-window ""))
   ""
   (("<right>" enlarge-window-horizontally ""))))

(e/bind [] ::f3-menu   ("<f3>")   'e/menu-fn::window-menu/body)


;;;
;; (F4) System and Files
;;

(defun e/close-buffer ()
  (interactive)
  (if (buffer-file-name (current-buffer))
      (save-buffer))
  (kill-buffer (current-buffer)))

(defun e/close-all-buffers ()
  (interactive)
  (save-some-buffers)
  (mapc 'kill-buffer (buffer-list)))

(e/bind []
  ::open              ("C-x C-f" "C-x f")          'find-file
  ::open-recent       ("C-r")                      'counsel-recentf
  ::open-project      ("ESC t" "M-t" "C-t" "s-t")  'projectile-find-file-dwim
  ::save              ("ESC s" "C-s" "M-s")        'save-buffer
  ::save-as           ()                           'write-file
  ::save-all          ("C-x C-s" "C-x s")          'save-some-buffers
  ::close             ("ESC `" "M-`" "C-`")        'e/close-buffer
  ::close-all         ()    'e/close-all-buffers
  ::directory         ("ESC 0" "M-0" "C-x 0" "C-x C-0" "C-0")   'treemacs)

(pretty-hydra-define e/menu-fn::file-menu
  (:color red :quit-key "z" :title "<F4> File")
  ("Bulk"
   (("1"  save-some-buffers    "save all")
    ("2"  e/close-all-buffers  "close all")
    ("D"  dired-jump "to dired" :exit t))
   
   "Save"
   (("s"  save-buffer "save")
    ("S"  write-file  "save as"))
   
   "Open"))

(e/bind [] ::f4-menu   ("<f4>")   'e/menu-fn::file-menu/body)


;;
;; (F5) System Toggles
;;

(pretty-hydra-define e/menu-fn::view-menu
  (:color amaranth :quit-key "z" :title "<F10> View")
  ("Basic"
   (("n" linum-mode "line number" :toggle t)
    ("w" whitespace-mode "whitespace" :toggle t)
    ("W" whitespace-cleanup-mode "whitespace cleanup" :toggle t)
    ("r" rainbow-mode "rainbow" :toggle t)
    ("L" page-break-lines-mode "page break lines" :toggle t))
   "Highlight"
   (("s" symbol-overlay-mode "symbol" :toggle t)
    ("l" hl-line-mode "line" :toggle t)
    ("x" highlight-sexp-mode "sexp" :toggle t)
    ("t" hl-todo-mode "todo" :toggle t))
   "UI"
   (("d" jp-themes-toggle-light-dark "dark theme" :toggle jp-current-theme-dark-p))
   "Coding"
   (("p" smartparens-mode "smartparens" :toggle t)
    ("P" smartparens-strict-mode "smartparens strict" :toggle t)
    ("S" show-smartparens-mode "show smartparens" :toggle t)
    ("f" flycheck-mode "flycheck" :toggle t))
   "Emacs"
   (("D" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
    ("X" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit)))))
    
(e/bind [] ::f10-menu   ("<f10>")   'e/menu-fn::view-menu/body)

(provide 'etude-core-bindings)
