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



(provide 'init-library)

;;; init-library.el ends here
