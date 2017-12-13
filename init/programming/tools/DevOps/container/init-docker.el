;;; init-docker.el --- init for Docker
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ docker ] -- Emacs interface to Docker.

(use-package docker
  :ensure t
  :ensure-system-package docker
  :bind (:map container-prefix
              ("c" . docker-containers)
              ("i" . docker-images)
              ("v" . docker-volumes)
              ("n" . docker-networks))
  :init
  (require 'docker-process)
  (setq docker-keymap-prefix "C-c t c")
  :config
  (setq docker-containers-show-all t)
  (docker-global-mode 1) ; enable global docker minor mode

  (autoload 'docker-read-container-name "docker-containers.el")
  (defun docker-insert-container (container-name)
    "A helper function to insert container ID or name."
    (interactive (list (docker-read-container-name "Docker container name: ")))
    (insert container-name))
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

  (defun docker-tramp-insert-running-container (container)
    "A helper function to insert the running `CONTAINER' name.
For Org-babel header argument :dir /docker:<name>:."
    (interactive (let ((containers-name (mapcar 'cdr (docker-tramp--running-containers)))
                       (containers-id (docker-tramp--running-containers)))
                   ;; `docker-tramp--running-containers' return a list of the form (ID NAME):
                   ;; '(("24dbb476cdd8"  "container_name")
                   ;;   ("bbd4f62e9f1d"  "other_name"))
                   ;; I think completing-read will recognize it as collection,
                   ;; but if you want to use the container names, you need to use:
                   ;; (mapcar 'cdr (docker-tramp--running-containers))
                   (list (completing-read "Docker container name: " containers-name))))
    (insert container))
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
