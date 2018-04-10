;;; init-prog-lang-query.el --- init Databases settings

;;; Commentary:



;;; Code:

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
