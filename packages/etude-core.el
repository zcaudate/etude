(use-packages (etude-base
               etude-company
  	       etude-dashboard
  	       etude-git
  	       etude-ivy
  	       etude-interactive
	       etude-lisp
  	       etude-neotree
  	       etude-programming
  	       etude-projectile
  	       etude-recentf
  	       etude-style
  	       etude-undo))

(require 'bind-key)
	      
(defmacro etude/bind-keys (&rest bindings)
  (let ((body (seq-mapcat (lambda (arg)
  			    (seq-map (lambda (binding)
  				       (list 'bind-key* binding (car arg)))
  				     (car (cdr arg))))
  			  (seq-partition bindings 2))))
    (cons 'progn body)))

(defun etude/do-nothing ()
  (interactive))

;; Keybinding Principles
;; - keys stay as close to the window mapping keys as possible
;; - `M-*` will be used instead of `s-*` for common purposes
;; - `C-*` version will be specified if possible to work on terminals

;; Menu rewrite
;; Instead of `M-x`, using alternate binding keys'
(progn
  (setq etude/menu         'counsel-M-x)
  (setq etude/git          'magit)
  (setq etude/describe-key 'describe-key)
  (setq etude/quit         'save-buffers-kill-terminal))

(etude/bind-keys
 etude/menu          ("C-p")
 etude/git           ("M-g" "C-x C-g")
 etude/describe-key  ("M-h")
 etude/quit          ("M-q" "C-x C-q"))

(defun etude/switch (name &optional command)
    (if (not (equal (buffer-name (current-buffer))
		    name))
	(if command
	    (eval command)
	  (switch-to-buffer name))
    (previous-buffer)))

(defun eshell-switch ()
  (interactive)
  (etude/switch "*eshell*" '(eshell)))

(defun file-tree-switch ()
  (interactive)
  (let ((cw (selected-window)))  ;; save current window
    (neotree-toggle)
    (select-window cw)))

(defun dashboard-switch ()
  (interactive)
  (etude/switch "*dashboard*"))

(defun scratch-switch ()
  (interactive)
  (etude/switch "*scratch*"))

(defun dashboard-close-all ()
  (interactive)
  (close-all-buffers)
  (dashboard-blank))

(progn
  (setq etude/shell-switch         'eshell-switch)
  (setq etude/file-tree-switch     'file-tree-switch)
  (setq etude/dashboard-switch     'dashboard-switch)
  (setq etude/scratch-switch       'scratch-switch)
  (setq etude/dashboard-close-all  'dashboard-close-all))

(etude/bind-keys
 etude/shell-switch           ("<f1>")
 etude/file-tree-switch       ("<f2>")
 etude/dashboard-switch       ("<f3>")
 etude/scratch-switch         ("<f4>"))

;; Text Editing
;; - Copy, Kill, Cut,
;; - Paste, List Paste 
;; - Undo, List Undo
;; - Redo, List Redo
	      
(defun paredit-copy-as-kill ()
  (interactive)
  (paredit-kill)
  (save-excursion (yank)))

(progn
  (setq etude/cut           'kill-region)
  (setq etude/copy          'copy-region-as-kill)
  (setq etude/cut-context   'paredit-kill)
  (setq etude/copy-context  'paredit-copy-as-kill)
  (setq etude/paste         'yank)
  (setq etude/paste-list    'counsel-yank-pop)
  (setq etude/undo          'undo)
  (setq etude/undo-list     'undo-tree-visualize)
  (setq etude/redo          'undo-tree-redo))

(etude/bind-keys
 etude/cut            ("C-a")
 etude/copy           ("C-i")
 etude/cut-context    ("C-k")
 etude/copy-context   ("C-j")
 etude/paste          ("C-y" "C-v")
 etude/paste-list     ("M-v" "C-x C-v")
 etude/undo           ("C--")
 etude/undo-list      ("M-_" "C-x C--")
 etude/redo           ("M--"))

;; List Editing
;; - Slurp and Burf using C-arrow
	      
;; Movement
;; - Simple Cursor Movements
	      
;; File
;; - Save File
;; - Create New File
;; - Open File
;; - Goto Directory
;; - Move File/Directory

(defhydra file-hydra (:color red :hint nil)
"
^File^                     ^Manage^
_1_: open file  
_2_: save file
_3_: save-all files 
_4_: close file           _q_: quit
"
  ("1" find-file)
  ("2" save-buffer)
  ("3" save-some-buffers)
  ("4" etude/do-nothing)
  ("q" etude/do-nothing :exit t))

(progn
  (setq etude/file-menu                  'file-hydra/body)
  (setq etude/file-open                  'find-file)
  (setq etude/file-open-recent           'etude/recentf-ivy-find-file)
  (setq etude/file-open-in-project       'projectile-find-file)
  (setq etude/file-save                  'save-buffer)
  (setq etude/file-save-all              'save-some-buffers)
  (setq etude/file-open-buffers          'switch-to-buffer)
  (setq etude/previous-buffer            'previous-buffer)
  (setq etude/next-buffer                'next-buffer)
  (setq etude/file-tree-locate-project   'neotree-project-dir)
  (setq etude/file-tree-locate-current   'neotree-locate-file))

(etude/bind-keys
 etude/file-menu                 ("M-1")
 etude/file-open                 ("C-o")
 etude/file-open-recent          ("C-r")
 etude/file-open-in-project      ("C-t")
 etude/file-save                 ("C-s")
 etude/file-save-all             ("C-x C-s")
 etude/file-open-buffers         ("C-b")
 etude/previous-buffer           ("M-/")
 etude/next-buffer               ("M-=")
 etude/file-tree-locate-project  ("C-l")
 etude/file-tree-locate-current  ("M-l" "C-x C-l"))

;; - Movement left and right within buffer
	      
;; - Movement between windows (using Meta)

(defun previous-window-current ()
  (interactive)
  (select-window (previous-window)))

(defun window-split-toggle ()
  (interactive)
  (let ((cnt (seq-count
	      (lambda (w)
		(not (equal (buffer-name (window-buffer w))
			    " *NeoTree*")))
	      (window-list))))
    (if (equal cnt 1)
	(split-window-below)
      (delete-other-windows))))

(defhydra window-hydra (:color red :hint nil)
"
^Position^                  ^Manage^                     Resize      
_1_: focus window           _o_: swap                      u 
_2_: window below           _t_: toggle                  l + r
_3_: window right           _d_: delete                    d
_4_: balance windows        _q_: quit     
"
  ("d" ace-delete-window)
  ("1" delete-other-windows)
  ("2" split-window-below)
  ("3" split-window-right)
  ("4" balance-windows)
  ("o" ace-swap-window)
  ("t" window-split-toggle)
  ("q" etude/do-nothing :exit t)
  ("<left>" shrink-window-horizontally)
  ("<up>" shrink-window)
  ("<down>" enlarge-window)
  ("<right>" enlarge-window-horizontally))

(progn
  (setq etude/window-focus    'delete-other-windows)
  (setq etude/window-menu     'window-hydra/body))

(etude/bind-keys
 etude/window-focus   ("M-RET")
 etude/window-menu    ("M-`"))

;; Search
;; - Search within buffer
;; - Search within project
;; - General search (specify folder)
;; - Search and replace
 	      
(progn
  (setq etude/search-buffer     'swiper)
  (setq etude/search-project    'counsel-ag))

(etude/bind-keys
 etude/search-buffer     ("C-f")
 etude/search-project    ("M-f" "C-x C-f"))

(provide 'etude-core)
