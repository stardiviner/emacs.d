;;; init-prog-lang-solidity.el --- init for Solidity

;;; Commentary:



;;; Code:

;;; [ solidity-mode ] -- Major mode for editing Solidity.

(use-package solidity-mode
  :ensure t
  :init
  (use-package company-solidity
    :ensure t
    :init
    ;; `company-solidity' is in `solidity-mode'
    (defun my/company-solidity-setup ()
      (my-company-add-backend-locally 'company-solidity))
    (add-hook 'solidity-mode-hook #'my/company-solidity-setup)))

;;; [ solidity-flycheck ] -- Flycheck integration for solidity emacs mode.

(use-package solidity-flycheck
  :ensure t)

;;; [ flymake-solidity ] -- A flymake handler for solidity using solc.

;; (use-package flymake-solidity
;;   :ensure t)



(provide 'init-prog-lang-solidity)

;;; init-prog-lang-solidity.el ends here
