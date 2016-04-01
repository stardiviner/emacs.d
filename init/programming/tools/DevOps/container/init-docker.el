;;; init-docker.el --- init for Docker
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ docker ] -- Emacs interface to Docker.

;;; Usage:
;;
;; - `docker-mode' / `docker-global-mode'
;; - `docker-*' :: command prefix.
;;   - `docker-images'
;;   - `docker-containers'
;;   - `docker-volumes'

(use-package docker
  :ensure t
  :config
  (define-key my-container-map (kbd "m") 'docker-mode)
  (define-key my-container-map (kbd "i") 'docker-images)
  (define-key my-container-map (kbd "c") 'docker-containers)
  (define-key my-container-map (kbd "v") 'docker-volumes)
  
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
  :ensure t
  :config
  (setq dockerfile-use-sudo nil)
  )


;;; [ docker-tramp ]

;;; Usage:
;;
;; 1. run a docker image.
;; 2. [C-x C-f] + /docker:user@container:/path/to/file

(use-package docker-tramp
  :ensure t
  :config
  (setq docker-tramp-use-names t
        docker-tramp-docker-options nil
        )

  ;; (defadvice tramp-completion-handle-file-name-all-completions
  ;;     (around dotemacs-completion-docker activate)
  ;;   "(tramp-completion-handle-file-name-all-completions \"\" \"/docker:\" returns
  ;;   a list of active Docker container names, followed by colons."
  ;;   (if (equal (ad-get-arg 1) "/docker:")
  ;;       (let* ((dockernames-raw (shell-command-to-string "docker ps | perl -we 'use strict; $_ = <>; m/^(.*)NAMES/ or die; my $offset = length($1); while(<>) {substr($_, 0, $offset, q()); chomp; for(split m/\\W+/) {print qq($_:\n)} }'"))
  ;;              (dockernames (cl-remove-if-not
  ;;                            #'(lambda (dockerline) (string-match ":$" dockerline))
  ;;                            (split-string dockernames-raw "\n"))))
  ;;         (setq ad-return-value dockernames))
  ;;     ad-do-it))
  )


(provide 'init-docker)

;;; init-docker.el ends here
