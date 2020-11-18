;;; init-GraphQL.el --- init for GraphQL
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ graphql ] -- GraphQL utilities.

(use-package graphql
  :ensure t
  :defer t)

;;; [ graphql-mode ] -- Major mode for editing GraphQL.

(use-package graphql-mode
  :ensure t
  :defer t)

;;; [ ob-graphql ] -- Org-Babel execution backend for GraphQL source blocks.

(use-package ob-graphql
  :ensure t
  :defer t
  :commands (org-babel-execute:graphql)
  :config
  (add-to-list 'org-babel-load-languages '(graphql . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))


(provide 'init-GraphQL)

;;; init-GraphQL.el ends here
