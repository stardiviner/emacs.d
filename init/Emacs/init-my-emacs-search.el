;;; init-my-emacs-search.el --- init search utilities for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(require 'init-my-emacs-search-isearch)
(require 'init-my-emacs-search-occur)
(require 'init-my-emacs-search-grep)
(require 'init-my-emacs-search-ack)
(require 'init-my-emacs-search-ag)
(require 'init-my-emacs-search-pt)
(require 'init-my-emacs-search-find)

(define-key my-search-prefix (kbd "s") 'ag)



(provide 'init-my-emacs-search)

;;; init-my-emacs-search.el ends here
