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

(use-package flycheck-ledger
  :ensure t
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
        (append '(("l" "[l]edger")
                  ;; Expense
                  ("le" "[E]xpenses 支出")
                  ("les" "[S]hopping 购物" plain
                   (file (lambda () (concat org-directory "/Accounting/finances.ledger")))
                   "%(org-read-date) %^{Event}\n expenses:shopping:%^{category}  %^{Amount}\n%?"
                   :empty-lines-before 1)
                  ("lef" "[F]ood 食物" plain
                   (file (lambda () (concat org-directory "/Accounting/finances.ledger")))
                   "%(org-read-date) %^{Event}\n expenses:food:%^{meat,breakfast,lunch,dinner}  %^{Amount}\n%?"
                   :empty-lines-before 1)
                  ("let" "[T]raffic 交通" plain
                   (file (lambda () (concat org-directory "/Accounting/finances.ledger")))
                   "%(org-read-date) %^{Event}\n expenses:traffic:%^{bus,train,plane}  %^{Amount}\n%?"
                   :empty-lines-before 1)
                  ("leh" "[H]ouse Rent 房租" plain
                   (file (lambda () (concat org-directory "/Accounting/finances.ledger")))
                   "%(org-read-date) %^{Event}\n expenses:house rent:  %^{Amount}\n%?"
                   :empty-lines-before 1)
                  ("lee" "[E]lectric fee 电费" plain
                   (file (lambda () (concat org-directory "/Accounting/finances.ledger")))
                   "%(org-read-date) %^{Event}\n expenses:house rent:  %^{Amount}\n%?"
                   :empty-lines-before 1)
                  
                  ;; Income
                  ("li" "[I]ncome 收入")
                  ("lis" "[S]alary 工资收入" plain
                   (file (lambda () (concat org-directory "/Accounting/finances.ledger")))
                   "%(org-read-date) %^{Event}\n income:salary:%^{account}  %^{Amount}\n%?"
                   :empty-lines-before 1)
                  ("lit" "[T]aobao 淘宝收入" plain
                   (file (lambda () (concat org-directory "/Accounting/finances.ledger")))
                   "%(org-read-date) %^{Event}\n income:salary:%^{account}  %^{Amount}\n%?"
                   :empty-lines-before 1)

                  ;; Transfer
                  ("lt" "[T]ransfer 转账")
                  ("ltb" "Take out money from [B]ank 从银行取钱"
                   (file (lambda () (concat org-directory "/Accounting/finances.ledger")))
                   "%(org-read-date) %^{Event}\n transfer:%^{source} -> %^{bank}  %^{Amount}\n%?"
                   :empty-lines-before 1)
                  ("lto" "save moeny on [o]nline account 存钱到虚拟账户"
                   (file (lambda () (concat org-directory "/Accounting/finances.ledger")))
                   "%(org-read-date) %^{Event}\n transfer:%^{source} -> %^{ZhiFuBao}  %^{Amount}\n%?"
                   :empty-lines-before 1)
                  ("ltc" "take out moeny to [C]ash 转账到现金"
                   (file (lambda () (concat org-directory "/Accounting/finances.ledger")))
                   "%(org-read-date) %^{Event}\n transfer:%^{source} -> cash  %^{Amount}\n%?"
                   :empty-lines-before 1)

                  ;; Debt
                  ("ld" "[D]ebt 债务")
                  ("ldr" "[R]ent 租金" plain
                   (file (lambda () (concat org-directory "/Accounting/finances.ledger")))
                   "%(org-read-date) %^{Event}\n debt:rent:%^{people}  %^{Amount}\n%?"
                   :empty-lines-before 1)
                  ("ldb" "[B]orrow 借贷" plain
                   (file (lambda () (concat org-directory "/Accounting/finances.ledger")))
                   "%(org-read-date) %^{Event}\n debt:borrow:%^{people}  %^{Amount}\n%?"
                   :empty-lines-before 1)
                  
                  ;; Assets
                  ("la" "[A]ssets 资产")
                  ("lab" "[B]ank 银行" plain
                   (file (lambda () (concat org-directory "/Accounting/finances.ledger")))
                   "%(org-read-date) %^{Event}\n assets:bank:%^{bank}  %^{Amount}\n%?"
                   :empty-lines-before 1)
                  ("lao" "[O]nline Accounts 虚拟账户" plain
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
;;   (add-hook 'hledger-mode-hook
;;             (lambda ()
;;               ;; for company-mode
;;               (my-company-add-backend-locally 'hledger-company)
;;               ;; for auto-complete
;;               (setq-local ac-sources '(hledger-ac-source))
;;               ))
;;
;;   ;; [ ob-hledger ]
;;   (require 'ob-hledger)
;;   (add-to-list 'org-babel-load-languages '(hledger . t))
;;   (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
;;   (add-to-list 'org-babel-tangle-lang-exts '("hledger" . "hledger")))



(provide 'init-tool-accounting)

;;; init-tool-accounting.el ends here
