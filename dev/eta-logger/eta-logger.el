;;; eta-logger.el --- key logger for emacs -*- lexical-binding: t -*-

;; Copyright (C) Chris Zheng

;; Author: Chris Zheng
;; Keywords: convenience, usability
;; Homepage: https://www.github.com/zcaudate/eta-logger
;; Package-Requires: ((emacs "25.1"))
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Eta provides two macros for standardising and associating
;; intent with key-bindings: a standard version `eta-bind` and
;; multi-dispatch version for standardisation of functionality across
;; major modes: `eta-modal-init` and `eta-modal`
;;

(defvar eta-logger-root "~/bin/keylogger")

(defvar eta-logger-day  (format-time-string "%Y-%m-%d"))

(defvar eta-logger-dir  (concat eta-logger-root "/" eta-logger-day))

(defvar eta-logger-file (format-time-string "%Y-%m-%d--%H.keylog"))

(defvar eta-logger-path
  (progn (when (not (file-exists-p eta-logger-dir))
           (make-directory eta-logger-dir))
         (concat eta-logger-dir "/" eta-logger-file)))

(defvar eta-logger-autosave-frequency 2)

(defvar eta-logger-timer nil
  "The timer")

(defvar eta-logger-excluded-modes '()
  "What modes to record data for.
Only include a mode if you're sure you won't type in any secrets!")

(defvar eta-logger-events '()
  "The data not yet saved in the logfile, newer first.")

(defun eta-logger-loggable-p ()
  (not (memq major-mode eta-logger-excluded-modes)))

(defun eta-logger-go ()
  (interactive)
  "Start logging Emacs commands and keystrokes."
  (add-hook 'pre-command-hook 'eta-logger-log)
  (add-hook 'kill-emacs-hook  'eta-logger-on-exit))

(defun eta-logger-on-exit ()
  (eta-logger-stop)
  (with-demoted-errors
      (eta-logger-save)))

(defun eta-logger-stop ()
  (interactive)
  "Stop logging Emacs commands and keystrokes."
  (when eta-logger-timer
    (cancel-timer eta-logger-timer)
    (setq eta-logger-timer nil))
  (remove-hook 'pre-command-hook 'eta-logger-log))

(defun eta-logger-clear-unsaved ()
  (interactive)
  "Erase that part of the log not yet written to the logfile."
  (setq eta-logger-events '()))

(defun eta-logger-log ()
  ;; (Would be `with-demoted-errors' instead if not for paranoia from
  ;; an actual user.)
  (ignore-errors
    (when (and (this-command-keys)
               (eta-logger-loggable-p))
      (push (vector (current-time)
                    (buffer-file-name)
                    (cons (line-number-at-pos)
                          (current-column))
                    (this-command-keys))
            eta-logger-events))))

(defun eta-logger-rotate ()
  (let ((ev (car (last eta-logger-events))))
    (let ((time  (aref ev 0)))
      (let ((day  (format-time-string "%Y-%m-%d" time)))
        (when (not (equal day eta-logger-day))
          (setq eta-logger-day day)
          (setq eta-logger-dir (concat eta-logger-root "/" day))
          (make-directory eta-logger-dir)))
      (setq eta-logger-file (format-time-string "%Y-%m-%d--%H.keylog" time))
      (setq eta-logger-path (concat eta-logger-dir "/" eta-logger-file)))))

(defun eta-logger-save ()
  (interactive)
  "Save the eta-logger buffer to eta-logger-path, and clear it."
  ;; N.B. not atomic
  (when eta-logger-events
    (eta-logger-rotate)
    (with-temp-buffer
      (dolist (k (nreverse eta-logger-events))
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
        (append-to-file nil nil eta-logger-path)
        (setq save-silently silent))
      (setq eta-logger-events '()))))

(defun eta-logger-autosave ()
  "Start the autosaver to save every key press"
  (setq eta-logger-timer
        (run-with-idle-timer eta-logger-autosave-frequency t
                             'eta-logger-save)))

(defun eta-logger-start ()
  (interactive)
  (eta-logger-go)
  (eta-logger-autosave))

(provide 'eta-logger)
