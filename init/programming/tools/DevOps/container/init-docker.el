;;; init-docker.el --- init for Docker
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ docker ] -- Emacs interface to Docker.

(use-package docker
  :ensure t
  :defer t
  :init
  (define-key my-container-map (kbd "m") 'docker-mode)
  (define-key my-container-map (kbd "i") 'docker-images)
  (define-key my-container-map (kbd "c") 'docker-containers)
  (define-key my-container-map (kbd "v") 'docker-volumes)
  :config
  (setq docker-containers-show-all t)
  )

;;; [ dockerfile-mode ] -- Dockerfile

(use-package dockerfile-mode
  :ensure t
  :defer t
  :config
  (setq dockerfile-use-sudo nil)
  )

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
