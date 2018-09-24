(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(use-package ace-window
  :ensure t)

(provide 'etude-interactive)
