;;; init-my-math-sage-math.el --- init for Sage Math.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ sage-shell-mode ] -- A front-end for Sage Math.

(use-package sage-shell-mode
  :ensure t
  :config
  (sage-shell:define-alias) ; define command `run-sage'.
  ;; (setq sage-shell:completion-function )
  ;; (setq sage-shell:completion-ignore-case t)
  ;; (setq sage-shell-edit:display-function)
  )


;;; [ ob-sagemath ] -- org-babel functions for SageMath evaluation.

(use-package ob-sagemath
  :ensure t
  :config
  ;; (setq ob-sagemath-output-display-function)
  ;; (setq org-babel-header-args:sage '())
  )


(provide 'init-my-math-sage-math)

;;; init-my-math-sage-math.el ends here
