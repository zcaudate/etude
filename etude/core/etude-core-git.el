(use-package magit :ensure t)

(use-package git-gutter
  :ensure t
  :diminish 'git-gutter-mode
  :config   (global-git-gutter-mode t))

(use-package git-timemachine :ensure t)

(use-package diff-hl :ensure t)

(pretty-hydra-define e/menu-fn::git-menu
  (:title "<F5> Git" :quit-key "z")
  ())

(defhydra+ e/menu-fn::git-menu ()
  ("<prior>" scroll-up-command)
  ("<next>" scroll-down-command)
  ("<up>" previous-line)
  ("<left>" left-char)
  ("<right>" right-char)
  ("<down>" next-line)
  ("C-<up>" previous-paragraph)
  ("C-<down>" next-paragraph))

(e/bind [] ::f5-menu   ("<f5>")   'e/menu-fn::git-menu/body)

(provide 'etude-core-git)

(defhydra hydra-timemachine
           (:hint nil 
            :post (progn (message "in post") (git-timemachine-quit))
            :body-pre (git-timemachine)
            :foreign-keys run )
    "Time machine"
    ("<up>" #'git-timemachine-show-previous-revision "Previous revision"
     :column "Navigation")
    ("<down>" #'git-timemachine-show-next-revision "Next revision")
    ("C-c h" #'git-timemachine-show-current-revision "Current revision")
    ("C-c C-c" "Quit" :color blue )
    ("C-c b" #'git-timemachine-blame "Show culprits" :column "Operations")
    ("C-c r" #'git-timemachine-kill-revision "Yank revision")
    ("C-c s" #'git-timemachine-kill-abbreviated-revision "Yank abbreviated revision"))
