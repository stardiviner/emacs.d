;;; init-prog-lang-database-mongodb.el --- init MongoDB

;;; Commentary:

;;; Code:

;;; [ mongo ]

(use-package mongo
  :ensure t
  :ensure-system-package mongodb
  :defer t)


;;; [ inf-mongo ] -- Run a MongoDB shell process in a buffer.

(use-package inf-mongo
  :ensure t
  :ensure-system-package mongodb-tools
  :defer t
  :init
  (define-key nosql-prefix (kbd "m") 'inf-mongo)
  )


;; [ ob-mongo ] -- Org-mode Babel for MongoDB

(use-package ob-mongo
  :ensure t
  :defer t
  :init
  (add-to-list 'org-babel-load-languages '(mongo . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  ;; FIXME: (add-to-list 'org-babel-tangle-lang-exts '("mongo" . "??"))
  )


(provide 'init-prog-lang-database-mongodb)

;;; init-prog-lang-database-mongodb.el ends here
