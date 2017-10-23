;;; init-kubernetes.el --- init for Kubernetes

;;; Commentary:



;;; Code:

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


(provide 'init-kubernetes)

;;; init-kubernetes.el ends here
