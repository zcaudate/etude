;;; Record all keystrokes, with timestamps at millisecond resolution.
;;; (Actually it's Emacs commands instead of keystrokes, but that's close enough.)

;;; Restricted to particular modes, but I really advise NOT RUNNING
;;; THIS AT ALL if you do much of anything sensitive inside Emacs.

(defvar keylogger-root "~/bin/keylogger")

(defvar keylogger-day  (format-time-string "%Y-%m-%d"))

(defvar keylogger-dir  (concat keylogger-root "/" keylogger-day))

(defvar keylogger-file (format-time-string "%Y-%m-%d--%H.keylog"))

(defvar keylogger-path
  (progn (when (not (file-exists-p keylogger-dir))
           (make-directory keylogger-dir))
         (concat keylogger-dir "/" keylogger-file)))

(defvar keylogger-autosave-frequency 2)

(defvar keylogger-timer nil
  "The timer")

(defvar keylogger-excluded-modes '()
  "What modes to record data for.
Only include a mode if you're sure you won't type in any secrets!")

(defvar keylogger-events '()
  "The data not yet saved in the logfile, newer first.")

(defun keylogger-loggable-p ()
  (not (memq major-mode keylogger-excluded-modes)))

(defun keylogger-go ()
  (interactive)
  "Start logging Emacs commands and keystrokes."
  (add-hook 'pre-command-hook 'keylogger-log)
  (add-hook 'kill-emacs-hook  'keylogger-on-exit))

(defun keylogger-on-exit ()
  (keylogger-stop)
  (with-demoted-errors
      (keylogger-save)))

(defun keylogger-stop ()
  (interactive)
  "Stop logging Emacs commands and keystrokes."
  (when keylogger-timer
    (cancel-timer keylogger-timer)
    (setq keylogger-timer nil))
  (remove-hook 'pre-command-hook 'keylogger-log))

(defun keylogger-clear-unsaved ()
  (interactive)
  "Erase that part of the log not yet written to the logfile."
  (setq keylogger-events '()))

(defun keylogger-log ()
  ;; (Would be `with-demoted-errors' instead if not for paranoia from
  ;; an actual user.)
  (ignore-errors
    (when (and (this-command-keys)
               (keylogger-loggable-p))
      (push (vector (current-time)
                    (buffer-file-name)
                    (cons (line-number-at-pos)
                          (current-column))
                    (this-command-keys))
            keylogger-events))))

(defun keylogger-rotate ()
  (let ((ev (car (last keylogger-events))))
    (let ((time  (aref ev 0)))
      (let ((day  (format-time-string "%Y-%m-%d" time)))
        (when (not (equal day keylogger-day))
          (setq keylogger-day day)
          (setq keylogger-dir (concat keylogger-root "/" day))
          (make-directory keylogger-dir)))
      (setq keylogger-file (format-time-string "%Y-%m-%d--%H.keylog" time))
      (setq keylogger-path (concat keylogger-dir "/" keylogger-file)))))

(defun keylogger-save ()
  (interactive)
  "Save the keylogger buffer to keylogger-path, and clear it."
  ;; N.B. not atomic
  (when keylogger-events
    (keylogger-rotate)
    (with-temp-buffer
      (dolist (k (nreverse keylogger-events))
        (let ((time (aref k 0))
              (mode (aref k 1))
              (pos  (aref k 2))
              (keys (aref k 3)))
          (insert (format "%s, %S, (%d, %d), %s\n"
                          time
                          mode
                          (car pos)
                          (cdr pos)
                          (if (symbolp keys)
                              keys
                            (key-description keys))))))
      (let ((silent save-silently))
        (setq save-silently t)
        (append-to-file nil nil keylogger-path)
        (setq save-silently silent))
      (setq keylogger-events '()))))

(defun keylogger-autosave ()
  "Start the autosaver to save every key press"
  (setq keylogger-timer
        (run-with-idle-timer keylogger-autosave-frequency t
                             'keylogger-save)))

(defun keylogger-start ()
  (interactive)
  (keylogger-go)
  (keylogger-autosave))

(provide 'keylogger)
