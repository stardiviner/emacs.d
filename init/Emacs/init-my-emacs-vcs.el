;;; init-my-emacs-vcs.el --- init Version Control System for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:

;;; vc

;;; Code:

;;; [ version control ]

(setq version-control t)                ; enable version control
(setq kept-new-versions 3)
(setq delete-old-versions t)
(setq kept-old-versions 2)
(setq dired-kept-versions 1)



;;; [ vc ]

(require 'vc)

;; (setq vc-follow-symlinks 'ask)


;;; Git

(require 'vc-git)



(provide 'init-my-emacs-vcs)

;;; init-my-emacs-vcs.el ends here
