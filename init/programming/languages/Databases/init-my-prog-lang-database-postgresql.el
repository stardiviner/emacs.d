;;; init-my-prog-lang-database-postgresql.el --- init for PostgresQL
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(add-hook 'sql-mode-hook 'sql-highlight-postgres-keywords)


;;; [ db-pg ] -- KeyValue Database on PostgreSQL.

;; Here's a small example that shows, at least, how to specify the connection details:
;;
;; ##! emacs-lisp
;; (let ((db
;;        (db-make
;;         '(db-pg
;;           :db "my-db" :username "nic"
;;           :table "t1" :column "c1" :key "a"))))
;;   (db-get "10" db))
;;
;; => '(("a" . 10)("b" . "20"))

;; (require 'db-pg)



;;; [ pg ]


;;; [ pgdevenv ]


;;; [ pgdevenv-el ]



;;; [ pgsrc-el ]



(provide 'init-my-prog-lang-database-postgresql)

;;; init-my-prog-lang-database-postgresql.el ends here
