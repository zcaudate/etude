(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(provide 'etude-interactive)
