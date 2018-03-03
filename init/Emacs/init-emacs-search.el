;;; init-emacs-search.el --- init search utilities for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'search-prefix)
  (define-prefix-command 'search-prefix))
(global-set-key (kbd "C-c s") 'search-prefix)

(require 'init-emacs-search-isearch)
(require 'init-emacs-search-occur)
(require 'init-emacs-search-grep)
(require 'init-emacs-search-wgrep)
(require 'init-emacs-search-ag)
;; (require 'init-emacs-search-pt)
;; (require 'init-emacs-search-ripgrep)
(require 'init-emacs-search-finder)
(require 'init-emacs-search-engine)

(with-eval-after-load 'counsel
  (define-key search-prefix (kbd "s") 'counsel-ag))


(provide 'init-emacs-search)

;;; init-emacs-search.el ends here
