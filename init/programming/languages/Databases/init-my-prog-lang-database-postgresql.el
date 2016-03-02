;;; init-my-prog-lang-database-postgresql.el --- init for PostgresQL
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(add-hook 'sql-mode-hook 'sql-highlight-postgres-keywords)


;;; [ db-pg ] -- key-value Database on PostgreSQL.

;; (use-package db-pg
;;   :ensure t
;;   )


;;; [ pg ] -- Emacs Lisp interface to the PostgreSQL RDBMS.

;; (use-package pg
;;   :ensure t
;;   )


;;; [ pgdevenv ] -- manage your PostgreSQL development envs.

;; (use-package pgdevenv
;;   :ensure t
;;   :config
;;   )


(provide 'init-my-prog-lang-database-postgresql)

;;; init-my-prog-lang-database-postgresql.el ends here
