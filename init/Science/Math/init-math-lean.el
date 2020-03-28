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
  :init
  (defun my/company-lean-setup ()
    (my-company-add-backend-locally 'company-lean))
  (add-hook 'lean-mode-hook #'my/company-lean-setup))

;;; [ helm-lean ]




(provide 'init-math-lean)

;;; init-math-lean.el ends here
