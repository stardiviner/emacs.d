;;; init-prog-lang-solidity.el --- init for Solidity

;;; Time-stamp: <2018-11-13 13:22:54 stardiviner>

;;; Commentary:



;;; Code:

;;; [ solidity-mode ] -- Major mode for editing Solidity.

(use-package solidity-mode
  :ensure t
  :config
  ;; `company-solidity' is in `solidity-mode'
  (add-hook 'solidity-mode-hook
            (lambda ()
              (my-company-add-backend-locally 'company-solidity))))

;;; [ solidity-flycheck ] -- Flycheck integration for solidity emacs mode.

(use-package solidity-flycheck
  :ensure t)

;;; [ flymake-solidity ] -- A flymake handler for solidity using solc.

;; (use-package flymake-solidity
;;   :ensure t)



(provide 'init-prog-lang-solidity)

;;; init-prog-lang-solidity.el ends here
