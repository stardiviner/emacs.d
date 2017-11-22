;;; init-container.el --- init for Container
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'container-prefix)
  (define-prefix-command 'container-prefix))
(define-key prog-tools-prefix (kbd "c") 'container-prefix)

;;; [ Docker ]

(require 'init-docker)

;;; [ lxc ] -- LXC integration with Emacs.

(use-package lxc
  :ensure t
  :defer t)

;;; [ Kubernetes ]

(require 'init-kubernetes)


(provide 'init-container)

;;; init-container.el ends here
