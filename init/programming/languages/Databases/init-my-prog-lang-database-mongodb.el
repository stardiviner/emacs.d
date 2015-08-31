;;; init-my-prog-lang-database-mongodb.el --- init MongoDB

;;; Commentary:

;;;; Code:

;;; [ mongo ]


;;; [ inf-mongo ] -- Run a MongoDB shell process in a buffer.

;;; Usage:
;;
;; - [M-x inf-mongo]

(require 'inf-mongo)

(define-key my-inferior-db-nosql-map (kbd "m") 'inf-mongo)


;;; [ mongo-elnode ]


(provide 'init-my-prog-lang-database-mongodb)

;;; init-my-prog-lang-database-mongodb.el ends here
