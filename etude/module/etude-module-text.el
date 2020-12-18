(ns: etude-module-text
  (:require
   etude-core
   etude-markdown-export))


(use-package dockerfile-mode
  :ensure t)
     
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(defvar e/strapdown-body
  "<!DOCTYPE html><html><title></title><xmp theme=\"journal\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>")

(defvar e/strapdown-zeta-body
  "<!DOCTYPE html><html><title></title><xmp theme=\"cosmo\" style=\"display:none;\"> %s  </xmp><script src=\"http://cdn.ztx.io/strapdown/strapdown.min.js\"></script>
   <style>.hljs{display:block;overflow-x:auto;padding:.5em;color:#333;background:#f8f8f8}.hljs-comment,.hljs-quote{color:#998;font-style:italic}.hljs-keyword,.hljs-selector-tag,.hljs-subst{color:#333;font-weight:700}.hljs-literal,.hljs-number,.hljs-tag .hljs-attr,.hljs-template-variable,.hljs-variable{color:teal}.hljs-doctag,.hljs-string{color:#d14}.hljs-section,.hljs-selector-id,.hljs-title{color:#900;font-weight:700}.hljs-subst{font-weight:400}.hljs-class .hljs-title,.hljs-type{color:#458;font-weight:700}.hljs-attribute,.hljs-name,.hljs-tag{color:navy;font-weight:400}.hljs-link,.hljs-regexp{color:#009926}.hljs-bullet,.hljs-symbol{color:#990073}.hljs-built_in,.hljs-builtin-name{color:#0086b3}.hljs-meta{color:#999;font-weight:700}.hljs-deletion{background:#fdd}.hljs-addition{background:#dfd}.hljs-emphasis{font-style:italic}.hljs-strong{font-weight:700}</style>
  </html>")

(defvar e/md-footer "<!-- Markdeep: --><style class='fallback'>body{visibility:hidden;white-space:pre;font-family:monospace}</style><script src='markdeep.min.js' charset='utf-8'></script><script src='https://morgan3d.github.io/markdeep/latest/markdeep.min.js' charset='utf-8'></script><script>window.alreadyProcessedMarkdeep||(document.body.style.visibility='visible')</script>")

(defun e/show-markdeep (buffer)
  (princ (with-current-buffer buffer
           (format "%s\n%s"
                   (buffer-string)
                   e/md-footer))
         (current-buffer)))

(defun e/show-markdown (buffer)
  (princ (with-current-buffer buffer
           (format e/stapdown-zeta-body))
         (current-buffer)))

(e/comment
    (with-current-buffer (get-buffer "INSTALL.org")
      (org-md_et-export-to-markdown))
  
  (e/show-org-markdown (get-buffer "INSTALL.org"))
  (with-current-buffer (get-buffer "INSTALL.org")
      (org-md_et-export-to-markdown)))


;;(type-of  (e/show-markdown-html (get-buffer "INSTALL.org")))

;;(e/show-md (get-buffer "INSTALL.org"))
;;(e/show-org-html (get-buffer "INSTALL.org"))
;;(e/show-org (get-buffer "INSTALL.org"))


(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")

(use-package impatient-mode :ensure t
  :init (setq impatient-mode-delay 3))

