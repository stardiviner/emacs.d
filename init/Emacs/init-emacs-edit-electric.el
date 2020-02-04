;;; init-emacs-edit-electric.el --- init for Electric
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ electric-indent-mode ]

;; (electric-indent-mode) ; globally
(add-hook 'prog-mode-hook #'electric-indent-local-mode) ; mode locally

;;; [ electric-pair-mode ]

;; (electric-pair-mode) ; globally
(add-hook 'prog-mode-hook #'electric-pair-local-mode) ; mode locally

(setq electric-pair-preserve-balance t
      electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit ; 'ignore
      electric-pair-skip-self t)

;;; [ electric-layout-mode ]



(provide 'init-emacs-edit-electric)

;;; init-emacs-edit-electric.el ends here
