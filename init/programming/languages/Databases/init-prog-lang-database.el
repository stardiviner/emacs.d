;;; init-prog-lang-database.el --- init Databases settings

;;; Commentary:



;;; Code:


(unless (boundp 'database-prefix)
  (define-prefix-command 'database-prefix))
(define-key inferior-prefix (kbd "d") 'database-prefix)

(unless (boundp 'sql-prefix)
  (define-prefix-command 'sql-prefix))
(define-key database-prefix (kbd "s") 'sql-prefix)

(unless (boundp 'nosql-prefix)
  (define-prefix-command 'nosql-prefix))
(define-key database-prefix (kbd "n") 'nosql-prefix)


;; SQL
(require 'init-prog-lang-database-sql)
(require 'init-prog-lang-database-sqlite)
(require 'init-prog-lang-database-mysql)
(require 'init-prog-lang-database-postgresql)

;; NewSQL
(require 'init-prog-lang-database-newsql)

;; NoSQL
(require 'init-prog-lang-database-nosql)
(require 'init-prog-lang-database-mongodb)
(require 'init-prog-lang-database-redis)

;;; CQL
(require 'init-prog-lang-database-cql)

;;; GraphQL
(require 'init-prog-lang-database-graphql)


(provide 'init-prog-lang-database)

;;; init-prog-lang-database.el ends here
