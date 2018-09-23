(use-package etude-ivy)

(defun etude/recentf-ivy-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ivy-completing-read "Open: " recentf-list nil t)))
    (when file
      (find-file file))))

(use-package recentf
  :ensure t
  :config
  (progn
    (require 'recentf)
    
    (setq recentf-max-saved-items 200
          recentf-max-menu-items 15)
    (recentf-mode +1)))
    
(provide 'etude-recentf)
