;;; init-my-prog-lang-database-nosql.el --- init for NoSQL
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ emacs-db ] -- very simple database for emacslisp, can also wrap other databases.

;; Emacs Db - Key/Values stores for Emacs
;;
;; An EmacsLisp interface to key/value stores (Mongo, Postgresql Hstore, etc..)
;; with a simple default implementation based on EmacsLisp Hashtables.
;;
;; The interface
;;
;; The idea behind this is to make an interface for interacting with simple
;; key/value database stores that is portable across all such stores. So you can
;; make code once but swap out the database with relative ease.
;;
;; Query language
;;
;; db uses the query language provided by the kv library, which is implemented
;; as a mapping function test on ever value by the persistent hashtable
;; implementation.
;;
;; The language should be translatable to just about any database query language
;; (Mongo, SQL, etc...).
;;
;; There are only 3 constructs currently, |, & and =.
;;
;; An expression could be:
;;   (= field-name value)

;;; Usage:
;;
;; - `db-make' [reference]
;; - `db-get' [key db]
;; - `db-put' [key value db]
;; - `db-map' [func db & optional query filter]
;; - `db-query' [db query]

(use-package db
  ;; :ensure t
  )


(provide 'init-my-prog-lang-database-nosql)

;;; init-my-prog-lang-database-nosql.el ends here
