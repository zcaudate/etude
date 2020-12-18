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
  ::help-key        ("ESC <f1>")                'helpful-key
  ::eval-elisp      ("ESC l")                   'eval-last-sexp)

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
    (bufler 0)))

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



;;;
;; (System and Files)
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
  ::close-all         ()                           'e/close-all-buffers
  ::directory         ("ESC 0" "M-0" "C-x 0" "C-x C-0" "C-0")   'treemacs)


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
  ::toggle-dired           ("ESC 4" "M-4")   'dired-jump)


;;
;; (F1) Command
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

(pretty-hydra-define e/menu-fn::start-menu
  (:title "<F1> Start" :quit-key "z"  :exit nil :foreign-keys run)
  ("Application"
   (("1" e/jump-to-terminal     "Terminal" :exit t)
    ("2" e/jump-to-start-screen "Dashboard" :exit t)
    ("3" e/jump-to-scratch      "Scratch" :exit t)
    ("4"  dired-jump "File Explorer" :exit t)
    ("Q" save-buffers-kill-terminal "Exit Emacs" :exit t))
   ""
   (("D" dash "Dash Docs" :exit t)
    ("T" tldr "TLDR" :exit t)
    ("B" wm3          "Browser" :exit t))
   "File"
   (("o o"  counsel-projectile-find-file-dwim   "open")
    ("o r"  counsel-recentf      "open recent")
    ("o p"  counsel-projectile   "open project")
    ("s a"  save-some-buffers    "save as")
    ("s s"  save-some-buffers    "save all"))
   ""
   (("f"  counsel-rg             "search text")
    ("c c"  e/close-buffer       "close")
    ("c A"  e/close-all-buffers  "close all"))
   "Window"
   (("H"   split-window-right   "split h" :exit nil)
    ("V"   split-window-below   "split v" :exit nil)
    ("DEL"   delete-window   "close")
    ("RET"   delete-other-windows "focus"))
   ""
   (("B"   'balance-windows-area "balance")
    ("D"   e/window-delete "delete")
    ("T"   e/split-window-toggle "toggle")
    ("W"   ace-swap-window "swap"))
   ""
   (("<up>"    shrink-window  "v-")
    ("<down>"  enlarge-window "v+")
    ("<left>"  shrink-window-horizontally "h-")
    ("<right>" enlarge-window-horizontally "h+"))
   "Help"
   (("i a" about-emacs   "About Emacs")
    ("i d" about-distribution   "About Distribution")
    ("i h" help-for-help "About Help")
    ("i b" describe-bindings  "Bound Keys")
    ("i m" describe-mode      "Current Mode"))))

(e/bind [] ::f1-menu   ("<f1>")   'e/menu-fn::start-menu/body)

(pretty-hydra-define e/menu-fn::settings-menu
  (:title "<F2> Settings" :quit-key "z" :exit nil :foreign-keys run)
  ("Package"
   (("P" package-list-packages  "List" :exit t)
    ("p H" describe-package  "Describe")
    ("p R" package-refresh-contents "Refresh")
    ("p I" package-install "Install")
    ("p D" package-delete "Delete"))
   "Customise"
   (("C" customize "all" :exit t)
    ("c f" customize-face  "face" :exit t)
    ("c t" customize-themes "theme" :exit t))
   "Toggle"
   (("L" display-line-numbers-mode "Line Numbers" :toggle t)
    ("V" visual-line-mode "Visual Line" :toggle t)
    ("W" whitespace-mode "Whitespace" :toggle t)
    ("H" auto-highlight-symbol-mode "Highlight Symbol" :toggle t))
   ""
   (("f s" flyspell-mode "Flyspell" :toggle t)
    ("f c" flycheck-mode "Flycheck" :toggle t)
    ("s p" smartparens-mode "Smartparens" :toggle t)
    ("s t" smartparens-strict-mode "Smartparens strict" :toggle t))))


(e/bind [] ::f2-menu   ("<f2>")   'e/menu-fn::settings-menu/body)

(pretty-hydra-define e/menu-fn::review-menu
  (:title "<F3> Review" :quit-key "z" :exit nil :foreign-keys run)
  ("Undo"
   (("u p" package-list-packages  "list" :exit t)
    ("u p" package-list-packages  "list" :exit t))
   "Annotate"
   (("A"    annotate-mode "Toggle" :toggle t)
    ("a a"  annotate-annotate "Add")
    ("a s"  annotate-show-annotation-summary "Prev")
    ("a p"  annotate-goto-previous-annotation "Next")
    ("a n"  annotate-goto-next-annotation "Prev"))
   ""
   (("a I"  annotate-integrate-annotations "Integrate")
    ("a X"  annotate-export-annotations "Export")
    ("a S"  annotate-save-annotations "Save")
    ("a C"  annotate-clear-annotations "Clear")
    ("a P"  annotate-purge-annotations "Purge"))
   "Hunks"
   (("H"  git-gutter:toggle  "Toggle" :toggle git-gutter-mode)
    ("h s" git-gutter:clear  "Select")
    ("h c" git-gutter:clear  "Clear")
    ("h n" git-gutter:next-hunk      "Next")
    ("h p" git-gutter:previous-hunk  "Prev"))
   ""
   (("h i" git-gutter:statistics   "Stats")
    ("h d" git-gutter:popup-hunk   "Diff")
    ("h r" git-gutter:revert-hunk  "Revert")
    ("h s" git-gutter:stage-hunk   "Stage"))
   "Git"
   (("L"  magit-log-all "Log")
    ("D"  magit-diff    "Diff"))
   ""
   (("C"  magit-commit  "Commit")
    
    ("F"  magit-pull    "Pull")
    ("P"  magit-push    "Push")
    ("S"  magit-status  "Status"))
   
   "Commands"
   (("h c" counsel-command-history "all" :exit t))))

(e/bind [] ::f2-menu   ("<f3>")   'e/menu-fn::review-menu/body)

(comment

 (ano)
  
 ("g t"  git-timemachine    "Timemachine"  :exit t)
 ("I" git-gutter:statistic      "File stats"))
