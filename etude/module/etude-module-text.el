(ns: etude-module-text
  (:require
   etude-core))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(defun on/show-markdown-html (buffer)
  (princ (with-current-buffer buffer
           (format "<!DOCTYPE html><html><title></title><xmp theme=\"simplex\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>"
                   (buffer-substring-no-properties (point-min) (point-max))))
         (current-buffer)))

(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")

(use-package impatient-mode :ensure t)
(use-package )
