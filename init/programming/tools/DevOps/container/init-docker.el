;;; init-docker.el --- init for Docker
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ docker ] -- Emacs interface to Docker.

(use-package docker
  :ensure t
  :bind (:map container-prefix
              ("c" . docker-containers)
              ("i" . docker-images)
              ("v" . docker-volumes)
              ("n" . docker-networks))
  :config
  (setq docker-containers-show-all t)
  (docker-global-mode 1) ; enable global docker minor mode
  )

;;; [ dockerfile-mode ] -- Dockerfile

(use-package dockerfile-mode
  :ensure t)

;;; [ docker-compose-mode ] -- Major mode for editing docker-compose files.

(use-package docker-compose-mode
  :ensure t)

;;; [ docker-tramp ]

(use-package docker-tramp
  :ensure t
  :config
  (setq docker-tramp-use-names t
        ;; docker-tramp-docker-options nil
        )
  )

;;; [ docker-api ] -- Emacs interface to the Docker API.

(use-package docker-api
  :ensure t)

;;; [ kubernetes ] -- Emacs porcelain for Kubernetes. A magit-style interface to the Kubernetes command-line client.

(use-package kubernetes
  :ensure t
  :commands (kubernetes-display-pods
             kubernetes-display-configmaps)
  )

;;; [ kubernetes-tramp ] -- offers a TRAMP method for Docker containers deployed in a Kubernetes cluster.

(use-package kubernetes-tramp
  :ensure t
  :config
  )


(provide 'init-docker)

;;; init-docker.el ends here
