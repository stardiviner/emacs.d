;;; init-MongoDB.el --- init MongoDB

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
  :commands (inf-mongo))


;; [ ob-mongo ] -- Org-mode Babel for MongoDB

(use-package ob-mongo
  :ensure t
  :defer t
  :commands (org-babel-execute:mongo)
  :config
  (add-to-list 'org-babel-load-languages '(mongo . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))


(provide 'init-MongoDB)

;;; init-MongoDB.el ends here
