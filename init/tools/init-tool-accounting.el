;;; init-tool-accounting.el --- init for Accounting in Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'accounting-prefix)
  (define-prefix-command 'accounting-prefix))
(define-key tools-prefix (kbd "a") 'accounting-prefix)

(use-package ledger-mode
  :ensure t
  :defer t
  :mode ("\\.ledger\\'" . ledger-mode))

(use-package company-ledger
  :ensure t
  :after ledger-mode
  :defer t
  :config
  (defun my-company-ledger-setup ()
    (my-company-add-backend-locally 'company-ledger))
  (add-hook 'ledger-mode-hook #'my-company-ledger-setup))

(use-package flycheck-ledger
  :ensure t
  :after ledger-mode
  :defer t
  :hook (ledger-mode . flycheck-mode)
  :config (add-hook 'ledger-mode-hook (lambda () (flycheck-select-checker 'ledger))))

;; [ ob-ledger ]

(use-package ob-ledger
  :defer t
  :commands (org-babel-execute:ledger)
  :config
  (add-to-list 'org-babel-load-languages '(ledger . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("ledger" . "ledger"))
  (setq org-babel-default-header-args:ledger
        '((:results . "output") (:cmdline . "bal"))))

(with-eval-after-load 'org-capture
  (setq org-capture-templates
        (append `(("l" ,(format "%s\tledger" (all-the-icons-faicon "money" :face 'all-the-icons-green)))
                  ;; Expense
                  ("le" ,(format "%s\tExpenses 支出"
                                 (all-the-icons-faicon "minus-circle" :face 'all-the-icons-green)))
                  ("les" ,(format "%s\tShopping 购物"
                                  (all-the-icons-faicon "shopping-cart" :face 'all-the-icons-green))
                   plain
                   (file (lambda () (concat org-directory "/Accounting/finances.ledger")))
                   "%(org-read-date) %^{Event}\n expenses:shopping:%^{category}  %^{Amount}\n%?"
                   :empty-lines-before 1)
                  ("lef" ,(format "%s\tFood 食物"
                                  (all-the-icons-material "restaurant" :face 'all-the-icons-green))
                   plain
                   (file (lambda () (concat org-directory "/Accounting/finances.ledger")))
                   "%(org-read-date) %^{Event}\n expenses:food:%^{meat,breakfast,lunch,dinner}  %^{Amount}\n%?"
                   :empty-lines-before 1)
                  ("let" ,(format "%s\tTraffic 交通"
                                  (all-the-icons-faicon "car" :face 'all-the-icons-purple-alt))
                   plain
                   (file (lambda () (concat org-directory "/Accounting/finances.ledger")))
                   "%(org-read-date) %^{Event}\n expenses:traffic:%^{bus,train,plane}  %^{Amount}\n%?"
                   :empty-lines-before 1)
                  ("leh" ,(format "%s\tHouse Rent 房租"
                                  (all-the-icons-faicon "home" :face 'all-the-icons-silver))
                   plain
                   (file (lambda () (concat org-directory "/Accounting/finances.ledger")))
                   "%(org-read-date) %^{Event}\n expenses:house rent:  %^{Amount}\n%?"
                   :empty-lines-before 1)
                  ("lee" ,(format "%s\tElectric fee 电费"
                                  (all-the-icons-material "power" :face 'all-the-icons-dsilver))
                   plain
                   (file (lambda () (concat org-directory "/Accounting/finances.ledger")))
                   "%(org-read-date) %^{Event}\n expenses:house rent:  %^{Amount}\n%?"
                   :empty-lines-before 1)
                  
                  ;; Income
                  ("li" ,(format "%s\tIncome 收入"
                                 (all-the-icons-faicon "plus-circle" :face 'all-the-icons-green)))
                  ("lis" ,(format "%s\tSalary 工资收入"
                                  (all-the-icons-material "receipt" :face 'all-the-icons-green))
                   plain
                   (file (lambda () (concat org-directory "/Accounting/finances.ledger")))
                   "%(org-read-date) %^{Event}\n income:salary:%^{account}  %^{Amount}\n%?"
                   :empty-lines-before 1)
                  ("lit" ,(format "%s\tTaobao 淘宝收入"
                                  (all-the-icons-faicon "plus-square-o" :face 'all-the-icons-green))
                   plain
                   (file (lambda () (concat org-directory "/Accounting/finances.ledger")))
                   "%(org-read-date) %^{Event}\n income:salary:%^{account}  %^{Amount}\n%?"
                   :empty-lines-before 1)
                  
                  ;; Transfer
                  ("lt" ,(format "%s\tTransfer 转账"
                                 (all-the-icons-faicon "money" :face 'all-the-icons-green)))
                  ("lto" ,(format "%s\tsave moeny on online account 存钱到虚拟账户"
                                  (all-the-icons-faicon "jpy" :face 'all-the-icons-green))
                   (file (lambda () (concat org-directory "/Accounting/finances.ledger")))
                   "%(org-read-date) %^{Event}\n transfer:%^{source} -> %^{ZhiFuBao}  %^{Amount}\n%?"
                   :empty-lines-before 1)
                  ("ltb" ,(format "%s\tTake out money from Bank 从银行取钱"
                                  (all-the-icons-faicon "money" :face 'all-the-icons-green))
                   (file (lambda () (concat org-directory "/Accounting/finances.ledger")))
                   "%(org-read-date) %^{Event}\n transfer:%^{source} -> %^{bank}  %^{Amount}\n%?"
                   :empty-lines-before 1)
                  ("ltc" ,(format "%s\ttake out Cash 提现"
                                  (all-the-icons-faicon "money" :face 'all-the-icons-green))
                   (file (lambda () (concat org-directory "/Accounting/finances.ledger")))
                   "%(org-read-date) %^{Event}\n transfer:%^{source} -> cash  %^{Amount}\n%?"
                   :empty-lines-before 1)
                  
                  ;; Debt
                  ("ld" ,(format "%s\tDebt 债务"
                                 (all-the-icons-faicon "money" :face 'all-the-icons-green)))
                  ("ldr" ,(format "%s\tRent 租金"
                                  (all-the-icons-faicon "money" :face 'all-the-icons-green))
                   plain
                   (file (lambda () (concat org-directory "/Accounting/finances.ledger")))
                   "%(org-read-date) %^{Event}\n debt:rent:%^{people}  %^{Amount}\n%?"
                   :empty-lines-before 1)
                  ("ldb" ,(format "%s\tBorrow 借贷"
                                  (all-the-icons-faicon "money" :face 'all-the-icons-green))
                   plain
                   (file (lambda () (concat org-directory "/Accounting/finances.ledger")))
                   "%(org-read-date) %^{Event}\n debt:borrow:%^{people}  %^{Amount}\n%?"
                   :empty-lines-before 1)
                  
                  ;; Assets
                  ("la" ,(format "%s\tAssets 资产"
                                 (all-the-icons-material "account_balance" :face 'all-the-icons-green)))
                  ("lab" ,(format "%s\tBank 银行"
                                  (all-the-icons-material "account_balance" :face 'all-the-icons-green))
                   plain
                   (file (lambda () (concat org-directory "/Accounting/finances.ledger")))
                   "%(org-read-date) %^{Event}\n assets:bank:%^{bank}  %^{Amount}\n%?"
                   :empty-lines-before 1)
                  ("lao" ,(format "%s\tOnline Accounts 虚拟账户"
                                  (all-the-icons-material "account_balance_wallet" :face 'all-the-icons-green))
                   plain
                   (file (lambda () (concat org-directory "/Accounting/finances.ledger")))
                   "%(org-read-date) %^{Event}\n assets:online-account:%^{ZhiFuBao}  %^{Amount}\n%?"
                   :empty-lines-before 1))
                org-capture-templates)))

;;; [ hledger-mode ] -- Major mode for editing hledger.

;; (use-package hledger-mode
;;   :ensure t
;;   :mode (("\\.journal\\'" . hledger-mode)
;;          ("\\.hledger\\'" . hledger-mode))
;;   :bind (:map accounting-prefix
;;               ("j" . hledger-run-command)
;;               ("e" . hledger-jentry))
;;   :config
;;   (setq hledger-jfile (expand-file-name (concat org-directory "/Accounting/hledger.journal")))
;;
;;   (defun my/hledger-mode-setup ()
;;     ;; for company-mode
;;     (my-company-add-backend-locally 'hledger-company)
;;     ;; for auto-complete
;;     (setq-local ac-sources '(hledger-ac-source)))
;;   (add-hook 'hledger-mode-hook #'my/hledger-mode-setup)
;;
;;   ;; [ ob-hledger ]
;;   (require 'ob-hledger)
;;   (add-to-list 'org-babel-load-languages '(hledger . t))
;;   (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
;;   (add-to-list 'org-babel-tangle-lang-exts '("hledger" . "hledger")))



(provide 'init-tool-accounting)

;;; init-tool-accounting.el ends here
