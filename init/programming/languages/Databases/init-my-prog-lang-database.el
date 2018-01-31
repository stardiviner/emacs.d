;;; init-my-prog-lang-database.el --- init Databases settings

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
(require 'init-my-prog-lang-database-sql)
(require 'init-my-prog-lang-database-sqlite)
(require 'init-my-prog-lang-database-mysql)
(require 'init-my-prog-lang-database-postgresql)

;; NewSQL
(require 'init-my-prog-lang-database-newsql)

;; NoSQL
(require 'init-my-prog-lang-database-nosql)
(require 'init-my-prog-lang-database-mongodb)
(require 'init-my-prog-lang-database-redis)

;;; CQL
(require 'init-my-prog-lang-database-cql)

;;; GraphQL
(require 'init-my-prog-lang-database-graphql)


(provide 'init-my-prog-lang-database)

;;; init-my-prog-lang-database.el ends here
