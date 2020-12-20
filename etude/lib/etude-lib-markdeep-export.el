(require 'etude-core)
(require 'ox-md)
(require 'dash)

(org-export-define-derived-backend 'markdeep 'md
  :menu-entry
  '(?y "Export to Markdeep"
       ((?M "To temporary buffer"
            (lambda (a s v b) (org-markdeep-export-as-markdeep a s v)))
        (?m "To file" (lambda (a s v b) (org-markdeep-export-to-markdeep a s v)))
        (?o "To file and open"
            (lambda (a s v b)
              (if a (org-markdeep-export-to-markdeep t s v)
                (org-open-file (org-markdeep-export-to-markdeep nil s v)))))))
  :translate-alist '((example-block . org-markdeep-example-block)
                     (fixed-width . org-markdeep-example-block)
                     (src-block . org-markdeep-example-block)))

(defun org-markdeep-star-border (text)
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

(defun org-markdeep-example-block (example-block _content info)
  "Transcode element EXAMPLE-BLOCK as ```lang ...'''."
  (let ((lang (org-element-property :language example-block))
        (body (org-remove-indentation
               (org-export-format-code-default example-block info))))
    (if (equal "md.graph" lang)
        (org-markdeep-star-border body)
      (format "```%s\n%s\n```" lang body))))

;;;###autoload
(defun org-markdeep-export-as-markdeep (&optional async subtreep visible-only)
  "See `org-md-export-as-markdeep'."
  (interactive)
  (org-export-to-buffer 'markdeep "*Org MD:Etude Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

;;;###autoload
(defun org-markdeep-convert-region-to-markdeep ()
  "See `org-md-convert-region-to-md'."
  (interactive)
  (org-export-replace-region-by 'markdeep))

;;;###autoload
(defun org-markdeep-export-to-markdeep (&optional async subtreep visible-only)
  "See `org-md-export-to-md'."
  (interactive)
  (let ((outfile (org-export-output-file-name ".md" subtreep)))
    (org-export-to-file 'markdeep outfile async subtreep visible-only)))

;;;###autoload
(defun org-markdeep-publish-to-markdeep (plist filename pub-dir)
  "Analogous to `org-md-publish-to-md'."
  (org-publish-org-to 'markdeep filename ".md" plist pub-dir))
  
(provide 'etude-lib-markdeep-export)
