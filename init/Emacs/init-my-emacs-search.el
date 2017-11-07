;;; init-my-emacs-search.el --- init search utilities for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'my-search-prefix)
  (define-prefix-command 'my-search-prefix))
(global-set-key (kbd "C-c s") 'my-search-prefix)

(require 'init-my-emacs-search-isearch)
(require 'init-my-emacs-search-occur)
(require 'init-my-emacs-search-grep)
(require 'init-my-emacs-search-wgrep)
;; (require 'init-my-emacs-search-ag)
;; (require 'init-my-emacs-search-pt)
(require 'init-my-emacs-search-ripgrep)
(require 'init-my-emacs-search-finder)
(require 'init-my-emacs-search-engine)

(with-eval-after-load 'counsel
  (define-key my-search-prefix (kbd "s") 'counsel-rg))


(provide 'init-my-emacs-search)

;;; init-my-emacs-search.el ends here
