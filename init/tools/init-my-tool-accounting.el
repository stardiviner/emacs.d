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
  )

;;; [ flycheck-ledger ]

(use-package flycheck-ledger
  :ensure t
  :defer t
  :init
  (add-hook 'ledger-mode-hook
            (lambda ()
              (flycheck-mode 1)
              (flycheck-select-checker 'ledger)))
  )

;;; ----------------------------------------------------------------------------

(provide 'init-my-tool-accounting)

;;; init-my-tool-accounting.el ends here
