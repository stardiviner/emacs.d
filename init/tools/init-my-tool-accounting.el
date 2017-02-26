;;; init-my-tool-accounting.el --- init for Accounting in Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------

(unless (boundp 'accounting-prefix)
  (define-prefix-command 'accounting-prefix))
(define-key my-tools-prefix (kbd "A") 'accounting-prefix)


(use-package ledger-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.ledger\\'" . ledger-mode))
  :config
  (use-package flycheck-ledger
    :ensure t
    :config
    (add-hook 'ledger-mode-hook
              (lambda ()
                (flycheck-mode 1)
                (flycheck-select-checker 'ledger)))
    )
  )

;;; [ hledger-mode ] -- Major mode for editing hledger.

(use-package hledger-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))
  (add-to-list 'auto-mode-alist '("\\.hledger\\'" . hledger-mode))
  (setq hledger-jfile "~/Org/Accounting/hledger.journal")
  :bind (:map accounting-prefix
              ("j" . hledger-run-command)
              ("e" . hledger-jentry))
  :config
  (add-hook 'hledger-mode-hook
            (lambda ()
              ;; for company-mode
              (my-company-add-backend-locally 'hledger-company)
              ;; for auto-complete
              (setq-local ac-sources '(hledger-ac-source))
              ))
  )

;;; ----------------------------------------------------------------------------

(provide 'init-my-tool-accounting)

;;; init-my-tool-accounting.el ends here
