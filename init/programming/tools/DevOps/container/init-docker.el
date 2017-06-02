;;; init-docker.el --- init for Docker
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ docker ] -- Emacs interface to Docker.

(use-package docker
  :ensure t
  :bind (:map my-container-map
              ("I" . docker-images)
              ("C" . docker-containers)
              ("V" . docker-volumes)
              ("N" . docker-networks))
  :config
  (setq docker-containers-show-all t)
  (docker-global-mode 1) ; enable docker minor mode
  )

;;; [ dockerfile-mode ] -- Dockerfile

(use-package dockerfile-mode
  :ensure t)

;;; [ docker-tramp ]

(use-package docker-tramp
  :ensure t
  :config
  (setq docker-tramp-use-names t
        docker-tramp-docker-options nil
        )
  )

;;; [ docker-api ] -- Emacs interface to the Docker API.

(use-package docker-api
  :ensure t)


(provide 'init-docker)

;;; init-docker.el ends here
