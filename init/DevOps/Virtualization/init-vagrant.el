;;; init-vagrant.el --- init for Vagrant
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'vagrant-prefix)
  (define-prefix-command 'vagrant-prefix))
(define-key prog-tools-prefix (kbd "v") 'vagrant-prefix)

;;; [ vagrant ] -- Manage a vagrant box from Emacs.

(use-package vagrant
  :ensure t
  :defer t
  :bind (:map vagrant-prefix
              ("u" . vagrant-up)
              ("s" . vagrant-ssh)
              ("t" . vagrant-tramp-term)
              ("S" . vagrant-status)
              ("P" . vagrant-suspend)
              ("r" . vagrant-resume)
              ("R" . vagrant-reload)
              ("h" . vagrant-halt)
              ("D" . vagrant-destroy)
              ("p" . vagrant-provision)
              ("e" . vagrant-edit))
  :custom ((vagrant-project-directory "~/Code/Vagrant/Arch")
           ;; (vagrant-up-options "")
           ))


;;; [ vagrant-tramp ] -- Vagrant method for TRAMP.

(use-package vagrant-tramp
  :ensure t
  :defer t)

;;; [ magrant ] -- A Vagrant porcelain in Emacs.

(use-package magrant
  :ensure t
  :defer t
  :commands (magrant magrant-boxes magrant-machines))


(provide 'init-vagrant)

;;; init-vagrant.el ends here
