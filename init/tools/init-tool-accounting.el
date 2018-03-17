;;; init-tool-accounting.el --- init for Accounting in Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'accounting-prefix)
  (define-prefix-command 'accounting-prefix))
(define-key tools-prefix (kbd "A") 'accounting-prefix)


(use-package ledger-mode
  :ensure t
  :ensure-system-package (ledger . "yaourt -S --noconfirm ledger")
  :defer t
  :mode ("\\.ledger\\'" . ledger-mode)
  :config
  (use-package flycheck-ledger
    :ensure t
    :config
    (add-hook 'ledger-mode-hook
              (lambda ()
                (flycheck-mode 1)
                (flycheck-select-checker 'ledger))))
  )

;; [ ob-ledger ]

(require 'ob-ledger)
(add-to-list 'org-babel-load-languages '(ledger . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-babel-tangle-lang-exts '("ledger" . "ledger"))

(setq org-babel-default-header-args:ledger
      '((:results . "output") (:cmdline . "bal")))

(with-eval-after-load 'org-capture
  (setq org-capture-templates
        (append '(("l" "[L]edger entries")
                  ;; Expense
                  ("le" "[E]xpenses")
                  ("les" "[S]hopping" plain
                   (file (concat org-directory "/Accounting/my.ledger"))
                   "%(org-read-date) %^{Event}\n expenses:shopping:%^{category}  %^{Amount}")
                  ("lef" "[F]ood" plain
                   (file (concat org-directory "/Accounting/my.ledger"))
                   "%(org-read-date) %^{Event}\n expenses:food:%^{meat,breakfast,lunch,dinner}  %^{Amount}")
                  ("let" "[T]raffic" plain
                   (file (concat org-directory "/Accounting/my.ledger"))
                   "%(org-read-date) %^{Event}\n expenses:traffic:%^{bus,train,plane}  %^{Amount}")
                  ("leh" "[H]ouse Rent" plain
                   (file (concat org-directory "/Accounting/my.ledger"))
                   "%(org-read-date) %^{Event}\n expenses:house rent:  %^{Amount}")
                  
                  ;; Income
                  ("li" "[I]ncome")
                  ("lis" "[S]alary" plain
                   (file (concat org-directory "/Accounting/my.ledger"))
                   "%(org-read-date) %^{Event}\n income:salary:%^{account}  %^{Amount}")

                  ;; Transfer
                  ("lt" "[T]ransfer")
                  ("ltb" "Take out money from [B]ank"
                   (file (concat org-directory "/Accounting/my.ledger"))
                   "%(org-read-date) %^{Event}\n transfer:%^{source} -> %^{bank}  %^{Amount}"
                   )
                  ("lto" "save moeny on [o]nline account"
                   (file (concat org-directory "/Accounting/my.ledger"))
                   "%(org-read-date) %^{Event}\n transfer:%^{source} -> %^{ZhiFuBao}  %^{Amount}"
                   )
                  ("ltc" "take out moeny to [C]ash"
                   (file (concat org-directory "/Accounting/my.ledger"))
                   "%(org-read-date) %^{Event}\n transfer:%^{source} -> cash  %^{Amount}"
                   )

                  ;; Debt
                  ("ld" "[D]ebt")
                  ("ldr" "[R]ent" plain
                   (file (concat org-directory "/Accounting/my.ledger"))
                   "%(org-read-date) %^{Event}\n debt:rent:%^{people}  %^{Amount}")
                  ("ldb" "[B]orrow" plain
                   (file (concat org-directory "/Accounting/my.ledger"))
                   "%(org-read-date) %^{Event}\n debt:borrow:%^{people}  %^{Amount}")
                  
                  ;; Assets
                  ("la" "[A]ssets")
                  ("lab" "[B]ank" plain
                   (file (concat org-directory "/Accounting/my.ledger"))
                   "%(org-read-date) %^{Event}\n assets:bank:%^{bank}  %^{Amount}")
                  ("lao" "[O]nline Accounts" plain
                   (file (concat org-directory "/Accounting/my.ledger"))
                   "%(org-read-date) %^{Event}\n assets:online-account:%^{ZhiFuBao}  %^{Amount}")
                  )
                org-capture-templates)))

;;; [ hledger-mode ] -- Major mode for editing hledger.

(use-package hledger-mode
  :ensure t
  :ensure-system-package (hledger hledger-api hledger-ui hledger-web)
  :mode (("\\.journal\\'" . hledger-mode)
         ("\\.hledger\\'" . hledger-mode))
  :bind (:map accounting-prefix
              ("j" . hledger-run-command)
              ("e" . hledger-jentry))
  :config
  (setq hledger-jfile (expand-file-name (concat org-directory "/Accounting/hledger.journal")))
  
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
  (add-to-list 'org-babel-tangle-lang-exts '("hledger" . "hledger"))
  )



(provide 'init-tool-accounting)

;;; init-tool-accounting.el ends here
