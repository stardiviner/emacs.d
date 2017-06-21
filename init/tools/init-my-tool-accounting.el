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
  :mode ("\\.ledger\\'" . ledger-mode)
  :config
  (use-package flycheck-ledger
    :ensure t
    :config
    (add-hook 'ledger-mode-hook
              (lambda ()
                (flycheck-mode 1)
                (flycheck-select-checker 'ledger)))
    )

  ;; [ ob-ledger ]
  (add-to-list 'org-babel-load-languages '(ledger . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  )

;;; [ hledger-mode ] -- Major mode for editing hledger.

(use-package hledger-mode
  :ensure t
  :mode (("\\.journal\\'" . hledger-mode)
         ("\\.hledger\\'" . hledger-mode))
  :bind (:map accounting-prefix
              ("j" . hledger-run-command)
              ("e" . hledger-jentry))
  :config
  (setq hledger-jfile "~/Org/Accounting/hledger.journal")
  
  (add-hook 'hledger-mode-hook
            (lambda ()
              ;; for company-mode
              (my-company-add-backend-locally 'hledger-company)
              ;; for auto-complete
              (setq-local ac-sources '(hledger-ac-source))
              ))

  ;; [ ob-hledger ]
  (require 'ob-hledger)
  (add-to-list 'org-babel-load-languages '(hledger . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  )

;;; ----------------------------------------------------------------------------

(provide 'init-my-tool-accounting)

;;; init-my-tool-accounting.el ends here
