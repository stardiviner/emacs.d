;;; init-emacs-edit-electric.el --- init for Electric
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Electric ]

(add-hook 'prog-mode-hook #'electric-indent-local-mode)
(add-hook 'prog-mode-hook #'electric-pair-local-mode)



(provide 'init-emacs-edit-electric)

;;; init-emacs-edit-electric.el ends here
