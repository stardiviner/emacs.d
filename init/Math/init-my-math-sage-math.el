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


;;; [ auto-complete-sage ] -- An auto-complete source for sage-shell-mode.

(use-package auto-complete-sage
  :ensure t
  :config
  (setq ac-sage-complete-on-dot nil
        ac-sage-show-quick-help t)
  
  (add-hook 'sage-shell:sage-mode-hook 'ac-sage-setup)
  (add-hook 'sage-shell-mode-hook 'ac-sage-setup)
  )


;;; [ helm-sage ]


;;; [ ob-sagemath ] -- org-babel functions for SageMath evaluation.

(use-package ob-sagemath
  :ensure t
  :config
  ;; (setq ob-sagemath-output-display-function)
  ;; (setq org-babel-header-args:sage '())

  ;; (with-eval-after-load "org"
  ;;   (define-key org-mode-map (kbd "C-c c") 'ob-sagemath-execute-async))
  )


(provide 'init-my-math-sage-math)

;;; init-my-math-sage-math.el ends here
