(ns: etude-module-org
  (:require etude-core etude-module-text))

(use-package org-cliplink :ensure t)

(defun on/org-fill-paragraph ()
  (interactive)
  (if (org-in-src-block-p)
      nil
    (org-fill-paragraph)))

(add-hook 'org-mode-hook '(lambda ()
                            (visual-line-mode)
                            (org-indent-mode)
                            (auto-fill-mode)
                            (setq org-confirm-babel-evaluate nil)
                            (define-key org-mode-map (kbd "<tab>") 'on/org-fill-paragraph)
                            (define-key org-mode-map (kbd "TAB")   'on/org-fill-paragraph)
                            (define-key org-mode-map (kbd "C-c C-g") 'org-cliplink)
                            (add-to-list 'org-src-lang-modes '("md.graph" . fundamental))
                            (add-to-list 'org-src-lang-modes '("bash" . shell))))

(defhydra on/org-mode-menu ()`
  ("9" on/preview-markdown  "preview markdown")
  ("0" on/preview-markdeep  "preview markdeep")
  ("x" on/preview-exit      "exit preview" :exit t))

(on/mode: [::org   org-mode "etude-module-org"]
  ::eval-cursor   'org-ctrl-c-ctrl-c
  ::mode-menu     'on/org-mode-menu/body)

;; Org JS Workaround
(setq org-babel-js-function-wrapper
      "process.stdout.write(require('util').inspect(function(){\n%s\n}(), { maxArrayLength: null, maxStringLength: null, breakLength: Infinity, compact: true }))")


;; Impatient mode filters
(defun on/show-org-markdeep (buffer)
  (save-window-excursion
    (with-current-buffer buffer
      (org-md_et-export-as-markdown))
    (princ (with-current-buffer (get-buffer "*Org MD:Etude Export*")
             (format "%s\n%s"
                     (buffer-string)
                     on/md-footer))
           (current-buffer))))

(defun on/show-org-markdown (buffer)
  (save-window-excursion
    (with-current-buffer buffer
      (org-md-export-as-markdown))
    (princ (with-current-buffer (get-buffer "*Org MD Export*")
             (format on/strapdown-zeta-body
                     (buffer-string)))
           (current-buffer))))

(defun on/show-org-html (buffer)
  (save-window-excursion
    (with-current-buffer buffer
      (org-html-export-as-html))
    (princ (with-current-buffer (get-buffer "*Org HTML Export*")
             (buffer-string))
           (current-buffer))))

(defun on/preview-markdeep ()
  (interactive)
  (if (not (httpd-running-p))
      (httpd-start))
  (impatient-mode t)
  (imp-set-user-filter 'on/show-org-markdeep)
  (browse-url (s-concat "http://localhost:8080/imp/live/"
                        (buffer-name (current-buffer)))))

(defun on/preview-markdown ()
  (interactive)
  (if (not (httpd-running-p))
      (httpd-start))
  (impatient-mode t)
  (imp-set-user-filter 'on/show-org-markdown)
  (browse-url (s-concat "http://localhost:8080/imp/live/"
                        (buffer-name (current-buffer)))))

(defun on/preview-exit ()
  (interactive)
  (if (imp-buffer-enabled-p (current-buffer))
      (impatient-mode)))

