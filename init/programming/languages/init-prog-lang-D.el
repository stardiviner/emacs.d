;;; init-prog-lang-D.el --- init for D programming language.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
;;; [ d-mode ] -- major mode for editing D code.

(use-package d-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode))
  )


;;; ----------------------------------------------------------------------------

(provide 'init-prog-lang-D)

;;; init-prog-lang-D.el ends here
