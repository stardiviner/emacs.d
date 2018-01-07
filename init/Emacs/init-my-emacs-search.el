;;; init-my-emacs-search.el --- init search utilities for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'search-prefix)
  (define-prefix-command 'search-prefix))
(global-set-key (kbd "C-c s") 'search-prefix)

(require 'init-my-emacs-search-isearch)
(require 'init-my-emacs-search-occur)
(require 'init-my-emacs-search-grep)
(require 'init-my-emacs-search-wgrep)
(require 'init-my-emacs-search-ag)
;; (require 'init-my-emacs-search-pt)
;; (require 'init-my-emacs-search-ripgrep)
(require 'init-my-emacs-search-finder)
(require 'init-my-emacs-search-engine)

(with-eval-after-load 'counsel
  (define-key search-prefix (kbd "s") 'counsel-ag))


(provide 'init-my-emacs-search)

;;; init-my-emacs-search.el ends here
