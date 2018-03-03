;;; init-prog-framework-ethereum.el --- init for Ethereum.

;;; Commentary:



;;; Code:

;;; [ solidity-mode ]

(use-package solidity-mode
  :ensure t
  :defer t)

;;; [ company-solidity ]

(use-package company-solidity
  :ensure t
  :defer t
  :config
  (defun my-company-solidity-setup ()
    (my-company-add-backend-locally 'company-solidity))
  (add-hook 'solidity-mode-hook #'my-company-solidity-setup)
  )



(provide 'init-prog-framework-ethereum)

;;; init-prog-framework-ethereum.el ends here
