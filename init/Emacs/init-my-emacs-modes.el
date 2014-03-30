;;; init-my-emacs-modes.el --- init Emacs modes settings
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; setup some auto-mode-alist
(add-to-list 'auto-mode-alist
             ;; Conky
             '("\\.conkyrc$" . conf-mode)
             '("conkyrc$" . conf-mode))

(provide 'init-my-emacs-modes)

;;; init-my-emacs-modes.el ends here
