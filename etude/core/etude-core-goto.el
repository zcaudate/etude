(require 'etude-lang)

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

(defun e/jump-to-eshell ()
  (interactive)
  (e/jump-to-buffer "*eshell*" 'eshell))

(defun e/jump-to-start-screen ()
  (interactive)
  (e/jump-to-buffer "*dashboard*" 'e/start-screen))

(defun e/jump-to-scratch ()
  (interactive)
  (e/jump-to-buffer "*scratch*" (lambda () (switch-to-buffer "*scratch*"))))

(e/bind []
  ::toggle-eshell          ("ESC 1" "M-1")                      'e/jump-to-eshell
  ::toggle-dashboard       ("ESC 2" "M-2")                      'e/jump-to-start-screen
  ::toggle-scratch         ("ESC 3" "M-3")                      'e/jump-to-scratch
  ::toggle-magit           ("ESC 4" "M-4")                      'magit
  ::toggle-workflow        ("ESC 5" "M-5")                      'e/jump-to-bindings
  ::toggle-treemacs        ("ESC 0" "M-0")                      'treemacs-find-file)
  
(provide 'etude-core-goto)