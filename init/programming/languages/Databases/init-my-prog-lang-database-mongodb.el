;;; init-my-prog-lang-database-mongodb.el --- init MongoDB

;;; Commentary:

;;; Code:

;;; [ mongo ]

(use-package mongo
  :ensure t
  :defer t)


;;; [ inf-mongo ] -- Run a MongoDB shell process in a buffer.

(use-package inf-mongo
  :ensure t
  :defer t
  :init
  (define-key my-inferior-db-nosql-map (kbd "m") 'inf-mongo)
  )


;; [ ob-mongo ] -- Org-mode Babel for MongoDB

(use-package ob-mongo
  :ensure t
  :config
  (add-to-list 'org-babel-load-languages '(mongo . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  ;; FIXME: (add-to-list 'org-babel-tangle-lang-exts '("mongo" . "??"))
  )


(provide 'init-my-prog-lang-database-mongodb)

;;; init-my-prog-lang-database-mongodb.el ends here
