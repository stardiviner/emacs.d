;;; init-container.el --- init for Container
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'my-container-map)
  (define-prefix-command 'my-container-map))
(define-key my-prog-tools-map (kbd "c") 'my-container-map)



(require 'init-docker)


;;; [ lxc ] -- LXC integration with Emacs.

(use-package lxc
  :ensure t)


(provide 'init-container)

;;; init-container.el ends here
