;;; init-emacs-vcs.el --- init Version Control System for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:

;;; vc

;;; Code:

;;; [ vc ]

(require 'vc)

(setq vc-handled-backends '(Git))
;; (setq vc-follow-symlinks 'ask)

(add-to-list 'display-buffer-alist
             '("\\*vc-diff\\*" . (display-buffer-below-selected)))


;;; Git

(require 'vc-git)



(provide 'init-emacs-vcs)

;;; init-emacs-vcs.el ends here
