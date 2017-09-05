;;; init-my-library.el --- some small utils / libraries for easy usage.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; get the content of region.

(defun region-content ()
  "Get the region content if region is active."
  (interactive)
  (if (use-region-p) ; `region-active-p'
      (buffer-substring-no-properties (region-beginning) (region-end))
    ))

;;; [ s.el ]

(use-package s
  :ensure t)

;;; [ dash.el ] -- A modern list library for Emacs.

(use-package dash
  :ensure t)

;;; [ a.el ] -- associative data structure functions.

(use-package a
  :ensure t)

;;; [ ht ] -- The missing hash table library for Emacs.

(use-package ht
  :ensure t)

;;; [ kv ] -- key/value data structure functions.

(use-package kv
  :ensure t)

;;; [ hierarchy ] -- Emacs library to create, query, navigate and display hierarchy structures.

(use-package hierarchy
  :ensure t)

;;; [ treepy ] --

(use-package treepy
  :ensure t)



(provide 'init-my-library)

;;; init-my-library.el ends here
