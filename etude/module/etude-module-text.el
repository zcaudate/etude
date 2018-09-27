(ns: etude-module-text
  (:require
   etude-core))

(use-package markdown-mode
  :defer true
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package yaml-mode
  :defer true
  :mode "\\.yml\\'")
