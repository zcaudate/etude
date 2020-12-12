(ns: etude-module-docker
  (:require etude-core))

(use-package docker
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package docker-compose-mode
  :ensure t)
