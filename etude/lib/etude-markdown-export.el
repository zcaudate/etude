(ns: etude-markdown-export
  (:require etude-core ox-md))

(org-export-define-derived-backend 'md_et 'md
  :menu-entry
  '(?y "Export to Markdeep"
       ((?M "To temporary buffer"
            (lambda (a s v b) (org-md_et-export-as-markdown a s v)))
        (?m "To file" (lambda (a s v b) (org-md_et-export-to-markdown a s v)))
        (?o "To file and open"
            (lambda (a s v b)
              (if a (org-md_et-export-to-markdown t s v)
                (org-open-file (org-md_et-export-to-markdown nil s v)))))))
  :translate-alist '((example-block . org-md_et-example-block)
                     (fixed-width . org-md_et-example-block)
                     (src-block . org-md_et-example-block)))

(defun org-md_et-star-border (text)
  (e/let [lines (s-lines text)
         llen  (-max (-map 'length lines))
         y-border (s-repeat (+ llen 2) "*")]
    (concat
     y-border "\n"
     (s-join "\n"
             (-map (lambda (s)
                     (s-concat "*" (s-pad-right llen " " s) "*"))
                   lines))
     "\n" y-border)))

;;(org-md_et-star-border "1\n1 -> 1\n1")

;;(-max (-map 'length  (s-lines )))

(defun org-md_et-example-block (example-block _content info)
  "Transcode element EXAMPLE-BLOCK as ```lang ...'''."
  (let ((lang (org-element-property :language example-block))
        (body (org-remove-indentation
               (org-export-format-code-default example-block info))))
    (if (equal "md.graph" lang)
        (org-md_et-star-border body)
      (format "```%s\n%s\n```" lang body))))

;;;###autoload
(defun org-md_et-export-as-markdown (&optional async subtreep visible-only)
  "See `org-md-export-as-markdown'."
  (interactive)
  (org-export-to-buffer 'md_et "*Org MD:Etude Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

;;;###autoload
(defun org-md_et-convert-region-to-md ()
  "See `org-md-convert-region-to-md'."
  (interactive)
  (org-export-replace-region-by 'md_et))

;;;###autoload
(defun org-md_et-export-to-markdown (&optional async subtreep visible-only)
  "See `org-md-export-to-markdown'."
  (interactive)
  (let ((outfile (org-export-output-file-name ".md" subtreep)))
    (org-export-to-file 'md_et outfile async subtreep visible-only)))

;;;###autoload
(defun org-md_et-publish-to-md (plist filename pub-dir)
  "Analogous to `org-md-publish-to-md'."
  (org-publish-org-to 'md_et filename ".md" plist pub-dir))
