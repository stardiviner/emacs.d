;;; init-my-prog-lang-database-mongodb.el --- init MongoDB

;;; Commentary:

;;;; Code:

;;; [ mongo ]


;;; [ inf-mongo ] -- Run a MongoDB shell process in a buffer.

;;; Usage:
;;
;; - [M-x inf-mongo]

(use-package inf-mongo
  :ensure t
  :config
  (define-key my-inferior-db-nosql-map (kbd "m") 'inf-mongo)
  )


;; [ ob-mongo ] -- Org-mode Babel for MongoDB

(use-package ob-mongo
  :ensure t
  :defer t
  )


;;; [ mongo-elnode ]


(provide 'init-my-prog-lang-database-mongodb)

;;; init-my-prog-lang-database-mongodb.el ends here
