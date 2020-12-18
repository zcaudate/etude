(use-package magit :ensure t)

(use-package git-gutter
  :ensure t
  :diminish 'git-gutter-mode
  :config   (global-git-gutter-mode t))

(use-package git-timemachine :ensure t)
  
(use-package diff-hl :ensure t)

(pretty-hydra-define e/menu-fn::git-menu
  (:title "<F5> Git" :quit-key "z")
  ("Version control"
   (("n" git-gutter:next-hunk      "Next hunk")
    ("p" git-gutter:previous-hunk  "Previous hunk")
    ("d" git-gutter:popup-hunk     "Show hunk diff")
    ("r" git-gutter:revert-hunk    "Revert hunk\n")
    ("c" git-gutter:stage-hunk     "Stage hunk")
    ("s" git-gutter:statistic      "How many added & deleted lines"))))

(e/bind [] ::f5-menu   ("<f5>")   'e/menu-fn::git-menu/body)

(provide 'etude-core-git)
