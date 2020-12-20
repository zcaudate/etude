;;
;; Java
;;
(require 'cedet)

(use-package jdee
  :defer t
  :config (setq jdee-server-dir (s-concat emacs-d "dev")))
  
(provide etude-module-java)