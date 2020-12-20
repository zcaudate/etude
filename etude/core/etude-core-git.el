(require 'etude-core-global)

(use-package magit :ensure t
  :config (add-hook 'git-commit-setup-hook
                    (lambda ()
                      (add-hook 'with-editor-post-finish-hook
                                (lambda ()
                        (call-interactively #'magit-push-current-to-upstream))
                                t t))))

(use-package git-gutter
  :ensure t
  :diminish 'git-gutter-mode
  :config   (global-git-gutter-mode t))

(use-package git-timemachine :ensure t)

(use-package diff-hl :ensure t)

(pretty-hydra-define e/menu-fn::git-timemachine-menu
  (:title "Git Timemachine" 
   :body-pre (git-timemachine)
   :quit-key "z"  :exit nil :foreign-keys run)
  ("Travel"
   (("p" git-timemachine-show-previous-revision "Prev")
    ("n" git-timemachine-show-next-revision  "Next")
    ("j" git-timemachine-show-nth-revision "To Nth")
    ("h" git-timemachine-help "Help")
    ("X" git-timemachine-quit "Exit"))
   ""
   (("L" git-timemachine-blame)
    ("T" git-timemachine-toggle)
    ("C" git-timemachine-show-commit)
    ("K" git-timemachine-kill-revision)
    ("B" git-timemachine-switch-branch))))

(defun e/git-timemachive-mode-menu ()
     (interactive)
     (if (eq hydra-curr-map e/menu-fn::git-timemachine-menu/keymap)
         (hydra-keyboard-quit)
       (e/menu-fn::git-timemachine-menu/body)))

(provide 'etude-core-git)
