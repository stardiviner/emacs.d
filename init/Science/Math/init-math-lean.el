;;; init-math-lean.el --- Lean Theorem Prover

;;; Commentary:



;;; Code:

;;; [ lean-mode ] -- Emacs mode for Lean.

(use-package lean-mode
  :ensure t
  :config
  ;; (setq lean-rootdir )
  )

;;; [ company-lean ]

(use-package company-lean
  :ensure t
  :config
  ;; (add-hook 'lean-mode-hook
  ;;           (lambda ()
  ;;             (my-company-add-backend-locally 'company-lean)))
  )

;;; [ helm-lean ]




(provide 'init-math-lean)

;;; init-math-lean.el ends here
