(require 'etude-lang)
(require 'etude-core-global)

;;
;; (F1) Help and References
;;

(pretty-hydra-define e/menu-fn::help-menu
  (:color yellow :quit-key "z" :title "<F1> Help")
  ("Emacs"
   (("0" about-emacs "about")
    ("1" describe-distribution "distribution"))
   
   "Helpful"
   (("f"  helpful-callable   "function")
    ("s"  helpful-symbol     "symbol")
    ("c"  helpful-command "command")
    ("d"  helpful-at-point "thing at point"))))


(pretty-hydra-define e/menu-fn::dired-menu
  (:title "<Mode> Dired")
  ("File"
   (("C" dired-do-copy "Copy")
    ("D" dired-do-delete "Delete")
    ("." dired-do-rename "Rename")
    ("L" dired-do-symlink "Symlink")
    ("R" dired-do-rsynch  "Rsync"))))

(e/)

(comment
 (defun no-op () nil)

 (define-key dired-mode-map (kbd  "C-c C-7") 'no-op))


(defhydra hydra-dired (:hint nil :color pink)
  "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
  ("\\" dired-do-ispell)
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" )        ;; Copy all marked files
  ("D" )
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("l" dired-do-redisplay)   ;; relist the marked or singel directory
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" )
  
  
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("." nil :color blue))

(define-key dired-mode-map "." 'hydra-dired/body)


(e/bind []
  ::f1-menu   ("<f1>")   'e/menu-fn::help-menu/body)




(e/comment
    (e/bind []
             
             ;;::view-variable       ()                     'describe-variable
             ;;::view-function       ()                     'describe-function
                                        ;::view-bindings       ()                     'describe-bindings
             ;;::view-package        ()                     'describe-package
             
             ;;::view-minor-modes    ()                     'counsel-minor
             ;;::view-major-modes    ()                     'counsel-major
             ;;::view-mode-info      ()                     'describe-mode
             ;;::conf-customise      ()                     'customize
             ;;::conf-workflow       ()                     'e/jump-to-workflow
             )

  (e/menu [::help-menu  ("<f1>")]
    "
  ^Help^                               ^Set[<0;54;16Mtings^
   _0_: Help for Help                  _u_: Customise Emacs
   _1_: About Emacs                    _w_: Customise Workflow
   _2_: About GNU            The workflow file define bindings as well as menu layouts. These work
   _3_: View Distribution Info         _8_: Major Modes   
   _4_: View Bindings                  _9_: Minor Modes
   _5_: View Function
   _6_: View Package                   _?_: Current Mode Info
   _7_: View Variable        
   "
    ("0" ::help-for-help :exit t)
    ("1" ::about-emacs)
    ("2" ::view-distribution)
    ("3" ::view-keystroke)
    ("4" ::view-bindings)
    ("5" ::view-function)
    ("6" ::view-package)
    ("7" ::view-variable)
    ("8" ::view-major-modes)
    ("9" ::view-minor-modes)
    ("?" ::view-mode-info)
    ("u" ::conf-customise nil :exit t)
    ("w" ::conf-workflow nil :exit t)
    ("z" ::do-nothing "cancel" :exit t)))


