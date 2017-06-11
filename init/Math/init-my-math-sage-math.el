;;; init-my-math-sage-math.el --- init for Sage Math.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


;;; [ sage-shell-mode ] -- A front-end for Sage Math.

(use-package sage-shell-mode
  :ensure t
  :config
  (setq sage-shell:use-prompt-toolkit t) ; temporarily fix prompt issue
  ;; (setq sage-shell:completion-function )
  ;; (setq sage-shell:completion-ignore-case t)
  ;; (setq sage-shell-edit:display-function)

  (sage-shell:define-alias) ; define command `run-sage'.
  )

;;; [ auto-complete-sage ] -- An auto-complete source for sage-shell-mode.

(use-package auto-complete-sage
  :ensure t
  :init
  (add-hook 'sage-shell:sage-mode-hook 'ac-sage-setup)
  (add-hook 'sage-shell-mode-hook 'ac-sage-setup)
  :config
  (setq ac-sage-complete-on-dot nil
        ac-sage-show-quick-help t)
  )

;;; [ helm-sage ]

(use-package helm-sage
  :ensure t)

;;; [ ob-sagemath ] -- org-babel functions for SageMath evaluation.

(use-package ob-sagemath
  ;; :ensure t
  :ensure ob-sage
  :config
  ;; (setq ob-sagemath-output-display-function)
  ;; (setq org-babel-header-args:sage '())

  (add-to-list 'org-babel-load-languages '(sagemath . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  ;; (with-eval-after-load "org"
  ;;   (define-key org-mode-map (kbd "C-c c") 'ob-sagemath-execute-async))
  )



(provide 'init-my-math-sage-math)

;;; init-my-math-sage-math.el ends here
