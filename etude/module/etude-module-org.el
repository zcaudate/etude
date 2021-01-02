(require 'etude-core)
(require 'etude-module-org-preview)

(use-package org-cliplink :ensure t)

(use-package plain-org-wiki :ensure t
  :init (setq plain-org-wiki-directory "~/.emacs.d/wiki"))

(use-package ob-async :ensure t)

(use-package ob-restclient :ensure t)

(defun e/org-fill-paragraph ()
  (interactive)
  (if (not (org-in-src-block-p))
      ;;(insert "\t")
    (org-fill-paragraph)))

(defun e/org-babel-tangle-block()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-babel-tangle)))

(defun org-babel-execute:dockerfile (body params)
  body)

(add-hook 'org-mode-hook '(lambda ()
                            (visual-line-mode)
                            (org-indent-mode)
                            (auto-fill-mode)
                            (setq org-confirm-babel-evaluate nil)
                            (define-key org-mode-map (kbd "<tab>") 'e/org-fill-paragraph)
                            (define-key org-mode-map (kbd "TAB")   'e/org-fill-paragraph)
                            (define-key org-mode-map (kbd "C-c C-g") 'org-cliplink)
                            (add-to-list 'org-src-lang-modes '("md.graph" . fundamental))
                            (add-to-list 'org-src-lang-modes '("bash" . shell))))


(defhydra e/org-mode-menu ()
  ("8" e/preview-html  "preview html")
  ("9" e/preview-markdown  "preview markdown")
  ("0" e/preview-markdeep  "preview markdeep")
  ("x" e/preview-exit      "exit preview" :exit t))

(defun e/org-fill-all ()
  (interactive)
  (save-excursion
    (mark-page)
    (fill-region (region-beginning) (region-end) nil)))

(e/mode [::org   org-mode "etude-module-org"]
  ::eval-cursor   'org-ctrl-c-ctrl-c
  ::eval-cursor-alt 'e/org-babel-tangle-block
  ::mode-connect  nil
  ::eval-file     'org-babel-tangle
  ::mode-menu     'e/org-mode-menu/body)

;; Org JS Workaround
(setq org-babel-js-function-wrapper
      "process.stdout.write(require('util').inspect(function(){\n%s\n}(), { maxArrayLength: null, maxStringLength: null, breakLength: Infinity, compact: true }))")


;; Impatient mode filters
(defun e/show-org-markdeep (buffer)
  (save-window-excursion
    (with-current-buffer buffer
      (org-markdeep-export-as-markdeep))
    (princ (with-current-buffer (get-buffer "*Org MD:Etude Export*")
             (format "%s\n%s"
                     (buffer-string)
                     e/md-footer))
           (current-buffer))))

(defun e/show-org-markdown (buffer)
  (save-window-excursion
    (with-current-buffer buffer
      (org-markdeep-export-as-markdeep))
    (princ (with-current-buffer (get-buffer "*Org MD Export*")
             (format e/strapdown-zeta-body
                     (buffer-string)))
           (current-buffer))))

(defun e/show-org-html (buffer)
  (save-window-excursion
    (with-current-buffer buffer
      (org-html-export-as-html))
    (princ (with-current-buffer (get-buffer "*Org HTML Export*")
             (buffer-string))
           (current-buffer))))

(defun e/preview-markdeep ()
  (interactive)
  (if (not (httpd-running-p))
      (httpd-start))
  (impatient-mode t)
  (imp-set-user-filter 'e/show-org-markdeep)
  (browse-url (s-concat "http://localhost:8080/imp/live/"
                        (buffer-name (current-buffer)))))

(defun e/preview-markdown ()
  (interactive)
  (if (not (httpd-running-p))
      (httpd-start))
  (impatient-mode t)
  (imp-set-user-filter 'e/show-org-markdown)
  (browse-url (s-concat "http://localhost:8080/imp/live/"
                        (buffer-name (current-buffer)))))

(defun e/preview-html ()
  (interactive)
  (if (not (httpd-running-p))
      (httpd-start))
  (impatient-mode t)
  (imp-set-user-filter 'e/show-org-html)
  (browse-url (s-concat "http://localhost:8080/imp/live/"
                        (buffer-name (current-buffer)))))


(defun e/preview-exit ()
  (interactive)
  (if (imp-buffer-enabled-p (current-buffer))
      (impatient-mode)))

(provide 'etude-module-org)
