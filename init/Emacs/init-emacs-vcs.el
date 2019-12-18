;;; init-emacs-vcs.el --- init Version Control System for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:

;;; vc

;;; Code:

;;; [ vc ]

(use-package vc
  :init (setq vc-handled-backends '(Git))
  (add-to-list 'display-buffer-alist
               '("\\*vc-diff\\*" . (display-buffer-below-selected))))


(provide 'init-emacs-vcs)

;;; init-emacs-vcs.el ends here
