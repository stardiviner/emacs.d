;;; init-emacs-edit-electric.el --- init for Electric
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ electric-indent-mode ]

(add-hook 'prog-mode-hook #'electric-indent-local-mode)

;;; [ electric-pair-mode ]

(add-hook 'prog-mode-hook #'electric-pair-local-mode)

(setq electric-pair-preserve-balance t
      electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit ; 'ignore
      electric-pair-skip-self t)



(provide 'init-emacs-edit-electric)

;;; init-emacs-edit-electric.el ends here
