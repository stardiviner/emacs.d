;;; init-my-emacs-color.el --- init for Color Manippulation
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ rainbow-mode ] -- colorize color names in buffers

(use-package rainbow-mode
  :ensure t
  :defer t
  :init
  (dolist (hook
           '(emacs-lisp-mode-hook
             css-mode-hook
             html-mode-hook))
    (add-hook hook (lambda () (rainbow-mode 1))))
  )


(provide 'init-my-emacs-color)

;;; init-my-emacs-color.el ends here
