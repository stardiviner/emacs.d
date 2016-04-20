;;; init-vagrant.el --- init for Vagrant
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ vagrant ] -- Manage a vagrant box from emacs.

(use-package vagrant
  :ensure t
  :config
  ;; (setq vagrant-project-directory "~/.vagrant.d")
  ;; (setq vagrant-up-options "")
  
  (unless (boundp 'my-vagrant-map)
    (define-prefix-command 'my-vagrant-map))
  (define-key my-prog-tools-map (kbd "v") 'my-vagrant-map)
  
  (define-key my-vagrant-map (kbd "l") 'vagrant-box-list)
  (define-key my-vagrant-map (kbd "u") 'vagrant-up)
  (define-key my-vagrant-map (kbd "s") 'vagrant-ssh)
  (define-key my-vagrant-map (kbd "t") 'vagrant-tramp-term)
  (define-key my-vagrant-map (kbd "S") 'vagrant-status)
  (define-key my-vagrant-map (kbd "P") 'vagrant-suspend)
  (define-key my-vagrant-map (kbd "r") 'vagrant-resume)
  (define-key my-vagrant-map (kbd "R") 'vagrant-reload)
  (define-key my-vagrant-map (kbd "h") 'vagrant-halt)
  (define-key my-vagrant-map (kbd "D") 'vagrant-destroy)
  (define-key my-vagrant-map (kbd "p") 'vagrant-provision)
  (define-key my-vagrant-map (kbd "e") 'vagrant-edit)
  )


;;; [ vagrant-tramp ] --

;;; Usage:
;;
;; The TRAMP method vagrant runs the vagrant-tramp-ssh script to get a list of
;; running Vagrant boxes used in the auto-complete function:
;;
;; C-x C-f /vagrant:
;; Find file: /vagrant:
;; -> devbox:
;;    esxbox:
;;    kafka-broker1:
;;    kafka-broker2:
;;    kafka-zookeeper:
;;
;; Opening a file as root
;;
;; Use this trick from the Emacs Wiki, where we replaced "ssh" with "vagrant"
;; and where "box" is a Vagrant box name:
;;
;; C-x C-f /vagrant:box|sudo:box:/path/to/file RET
;;
;; vagrant-tramp-term
;;
;; The vagrant-tramp-term function uses vagrant-tramp-ssh to provide a list of
;; completions, selection upon which will vagrant ssh to the given box using an
;; ansi-term.
;;
;; M-x vagrant-tramp-term
;;
;; vagrant ssh to box:
;; -> devbox:
;;    kafka-broker1:
;;    kafka-broker2:

(use-package vagrant-tramp
  :ensure t
  )


(provide 'init-vagrant)

;;; init-vagrant.el ends here
