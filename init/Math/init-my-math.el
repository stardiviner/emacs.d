;;; init-my-math.el --- init for Mathematics
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(require 'init-my-math-proof-assistant)


;;; [ Sage Math ]

(require 'init-my-math-sage-math)


;;; [ GMPL (MathProg) ] -- GMPL(MathProg) files

(use-package gmpl-mode
  :ensure t)



(provide 'init-my-math)

;;; init-my-math.el ends here
