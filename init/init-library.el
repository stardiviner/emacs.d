;;; init-library.el --- some small utils / libraries for easy usage.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ s.el ]

(use-package s
  :ensure t
  :defer t)

;;; [ dash.el ] -- A modern list library for Emacs.

(use-package dash
  :ensure t
  :defer t)

;;; [ a.el ] -- associative data structure functions.

(use-package a
  :ensure t
  :defer t)

;;; [ ht ] -- The missing hash table library for Emacs.

(use-package ht
  :ensure t
  :defer t)

;;; [ kv ] -- key/value data structure functions.

(use-package kv
  :ensure t
  :defer t)

;;; [ hierarchy ] -- Emacs library to create, query, navigate and display hierarchy structures.

(use-package hierarchy
  :ensure t
  :defer t)

;;; [ treepy ] -- Generic tree traversal tools.

(use-package treepy
  :ensure t
  :defer t)

;;; [ aio ] -- async/await for Emacs Lisp.

;; (use-package aio
;;   :ensure t)

;;; [ deferred ] -- Simple asynchronous functions for Emacs Lisp.

(use-package deferred
  :ensure t)

;;; [ async ] -- Simple library for asynchronous processing in Emacs.

;; (use-package async
;;   :ensure t)

;;; [ pfuture ] -- a simple wrapper around asynchronous processes.

;; (use-package pfuture
;;   :ensure t)

;;; [ promise ] -- A simple implementation of Promises/A+.

;; (use-package promise
;;   :ensure t)

;;; [ lcr ] -- Codinig with explicit contination arguments is a well-known technique, called continuation-passing style (CPS).

;; (use-package lcr
;;   :ensure t)



(provide 'init-library)

;;; init-library.el ends here
