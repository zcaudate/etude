(ns: etude-module-native
  (:require
   etude-core))

(use-package opencl-mode
  :defer t
  :commands (opencl-mode gfm-mode)
  :mode (("\\.cl\\'" . opencl-mode)))
