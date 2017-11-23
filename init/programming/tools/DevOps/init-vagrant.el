;;; init-vagrant.el --- init for Vagrant
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'vagrant-prefix)
  (define-prefix-command 'vagrant-prefix))
(define-key prog-tool-prefix (kbd "v") 'vagrant-prefix)

;;; [ vagrant ] -- Manage a vagrant box from Emacs.

(use-package vagrant
  :ensure t
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
              ("e" . vagrant-edit)
              )
  :config
  (setq vagrant-project-directory "~/Code/Vagrant/Arch")
  ;; (setq vagrant-up-options "")
  )


;;; [ vagrant-tramp ] --

(use-package vagrant-tramp
  :ensure t)


(provide 'init-vagrant)

;;; init-vagrant.el ends here
