;;; init-my-prog-tools-container-docker.el --- init for Docker
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'my-prog-tools-container-map)
  (define-prefix-command 'my-prog-tools-container-map))
(define-key my-prog-tools-map (kbd "c") 'my-prog-tools-container-map)


;;; [ docker ] -- Emacs interface to Docker.

;;; Usage:
;;
;; - `docker-mode' / `docker-global-mode'
;; - `docker-*' :: command prefix.
;;   - `docker-images'
;;   - `docker-containers'
;;   - `docker-volumes'

(use-package docker
  :init
  ;; FIXME: this does not work.
  ;; (setq docker-keymap-prefix "C-c t c")
  :config
  (define-key my-prog-tools-container-map (kbd "m") 'docker-mode)
  (define-key my-prog-tools-container-map (kbd "i") 'docker-images)
  (define-key my-prog-tools-container-map (kbd "c") 'docker-containers)
  (define-key my-prog-tools-container-map (kbd "v") 'docker-volumes)
  
  ;; (docker-global-mode)
  )


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

(use-package dockerfile-mode
  :config
  (setq dockerfile-use-sudo nil)
  )


;;; [ docker-tramp ]

;;; Usage:
;;
;; 1. run a docker image.
;; 2. [C-x C-f] + /docker:[IMAGE]:/path/to/file

(use-package docker-tramp
  :config
  (setq docker-tramp-docker-options nil
        docker-tramp-use-names nil
        )
  )


(provide 'init-my-prog-tools-container-docker)

;;; init-my-prog-tools-container-docker.el ends here
