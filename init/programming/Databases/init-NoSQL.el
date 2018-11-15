;;; init-NoSQL.el --- init for NoSQL
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ emacs-db ] -- very simple database for emacslisp, can also wrap other databases.

;; (use-package db
;;   :ensure t
;;   :defer t)

(require 'init-Redis)
(require 'init-MongoDB)
(require 'init-CouchDB)
(require 'init-Neo4j)


(provide 'init-NoSQL)

;;; init-NoSQL.el ends here
