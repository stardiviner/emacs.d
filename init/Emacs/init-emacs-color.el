;;; init-emacs-color.el --- init for Color Manippulation
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ rainbow-mode ] -- colorize color names in buffers

(use-package rainbow-mode
  :ensure t
  :defer t
  :delight rainbow-mode
  :init
  (dolist (hook
           '(emacs-lisp-mode-hook
             css-mode-hook
             html-mode-hook))
    (add-hook hook #'rainbow-mode)))


(provide 'init-emacs-color)

;;; init-emacs-color.el ends here
