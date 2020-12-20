
(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")

(use-package dockerfile-mode
  :ensure t)
   
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
  
(use-package graphviz-dot-mode :ensure t)

(provide 'etude-core-filetype)