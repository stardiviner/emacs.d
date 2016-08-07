;;; init-my-math-sage-math.el --- init for Sage Math.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ sage-shell-mode ] -- A front-end for Sage Math.

(use-package sage-shell-mode
  :ensure t)


;;; [ ob-sagemath ] -- org-babel functions for SageMath evaluation.

(use-package ob-sagemath
  :ensure t)


(provide 'init-my-math-sage-math)

;;; init-my-math-sage-math.el ends here
