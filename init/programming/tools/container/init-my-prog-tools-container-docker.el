;;; init-my-prog-tools-container-docker.el --- init for Docker
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ docker ]




;;; [ dockerfile-mode ] -- Dockerfile

;;; Usage:
;;
;; - [C-c C-b] :: build docker image from the buffer.
;;
;; You can specify the image name in the file itself by adding a line like this at the top of your `Dockerfile'.
;;
;; ## -*- docker-image-name: "your-image-name-here" -*-
;;
;; If you don't, you'll be prompted for an image each time you build.

(require 'dockerfile-mode)

(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))


;;; [ docker-tramp ]

;;; Usage:
;;
;; -

(require 'docker-tramp)


(provide 'init-my-prog-tools-container-docker)

;;; init-my-prog-tools-container-docker.el ends here
