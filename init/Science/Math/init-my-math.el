;;; init-my-math.el --- init for Mathematics
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Proof Assistant ]

;; (require 'init-my-math-proof-assistant)

;;; [ company-math ] -- completion backends for unicode math symbols and latex tags.

(use-package company-math
  :ensure t
  :config
  (defun my-company-math-setup ()
    (make-local-variable 'company-backends)
    (add-to-list 'company-backends 'company-math-symbols-latex t)
    ;; (append company-backends 'company-math-symbols-unicode)
    )
  (add-hook 'org-mode-hook 'my-company-math-setup)
  (add-hook 'LaTeX-mode-hook 'my-company-math-setup)
  )

;;; [ Sage Math ]

;; (require 'init-my-math-sage-math)


;;; [ GMPL (MathProg) ] -- GMPL(MathProg) files

(use-package gmpl-mode
  :ensure t
  :defer t)


;;; [ Wolfram ]

(require 'init-my-math-wolfram)

;;; [ Lean Theorem Prover ]

;; (require 'init-my-math-lean)



(provide 'init-my-math)

;;; init-my-math.el ends here
