;;; init-my-prog-lang-database.el --- init Databases settings

;;; Commentary:



;;; Code:


(unless (boundp 'my-inferior-db-map)
  (define-prefix-command 'my-inferior-db-map))
(define-key my-prog-inferior-map (kbd "d") 'my-inferior-db-map)

(unless (boundp 'my-inferior-db-sql-map)
  (define-prefix-command 'my-inferior-db-sql-map))
(define-key my-inferior-db-map (kbd "s") 'my-inferior-db-sql-map)

(unless (boundp 'my-inferior-db-nosql-map)
  (define-prefix-command 'my-inferior-db-nosql-map))
(define-key my-inferior-db-map (kbd "n") 'my-inferior-db-nosql-map)


(require 'init-my-prog-lang-database-sql)
(require 'init-my-prog-lang-database-sqlite)
(require 'init-my-prog-lang-database-mysql)
(require 'init-my-prog-lang-database-postgresql)

(require 'init-my-prog-lang-database-nosql)
(require 'init-my-prog-lang-database-mongodb)
(require 'init-my-prog-lang-database-redis)


(provide 'init-my-prog-lang-database)

;;; init-my-prog-lang-database.el ends here
