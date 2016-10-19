;;; init-vagrant.el --- init for Vagrant
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ vagrant ] -- Manage a vagrant box from Emacs.

(use-package vagrant
  :ensure t
  :defer t
  :init
  (unless (boundp 'my-vagrant-map)
    (define-prefix-command 'my-vagrant-map))
  (define-key my-prog-tools-map (kbd "v") 'my-vagrant-map)
  
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
  
  :config
  (setq vagrant-project-directory "~/Code/Vagrant/Arch")
  ;; (setq vagrant-up-options "")
  )


;;; [ vagrant-tramp ] --

(use-package vagrant-tramp
  :ensure t)


(provide 'init-vagrant)

;;; init-vagrant.el ends here
