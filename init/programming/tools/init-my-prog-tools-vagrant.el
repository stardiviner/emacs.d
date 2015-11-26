;;; init-my-prog-tools-vagrant.el --- init for Vagrant
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ vagrant ] -- Manage a vagrant box from emacs.

;;; Usage:
;;
;; Synopsis:
;;
;; Manage Vagrant boxes from Emacs.
;;
;; This package lets you send vagrant commands while working within a project containing a Vagrantfile.
;;
;; It will traverse the directory tree until a Vagrantfile is found and assume
;; this is the box you want to work with. It can be handy to bring a box up,
;; (re)provision, or even ssh to without leaving emacs.
;;
;; The emacs command vagrant-up will run vagrant up in a shell, other commands
;; follow the pattern vagrant-X emacs command runs vagrant X in the shell. An
;; exception is vagrant-edit, which will open the Vagrantfile for editing.
;;
;; When vagrant commands are given a prefix, you will be prompted for a box
;; name. For example: [C-u M-x vagrant-up]



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
  )


(provide 'init-my-prog-tools-vagrant)

;;; init-my-prog-tools-vagrant.el ends here
