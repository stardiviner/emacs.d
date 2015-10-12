;;; init-my-emacs-search.el --- init search utilities for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ helm-recoll ] -- full text search tool based on Xapian backend.

;;; Usage:
;;
;; -

;;; You need to create some helm-recoll sources before you can use them. You can
;;; create sources using the `helm-recoll-create-source' function, e.g. like
;;; this:
;; (helm-recoll-create-source "docs" "~/.recoll/docs")
;; (helm-recoll-create-source "progs" "~/.recoll/progs")

;;; Then you can use the sources in helm like this:
;; (helm :sources '(helm-source-recoll-docs helm-source-recoll-progs :buffer *helm recoll*))



(require 'init-my-emacs-search-isearch)
(require 'init-my-emacs-search-occur)
(require 'init-my-emacs-search-grep)
(require 'init-my-emacs-search-ack)
(require 'init-my-emacs-search-ag)
(require 'init-my-emacs-search-pt)

(define-key my-search-prefix (kbd "s") 'ag-regexp)


(unless (boundp 'my-search-language-prefix)
  (define-prefix-command 'my-search-language-prefix))
(define-key my-search-prefix (kbd "l") 'my-search-language-prefix)

(require 'init-my-emacs-search-pinyin)
(require 'init-my-emacs-search-migemo)


(provide 'init-my-emacs-search)

;;; init-my-emacs-search.el ends here
