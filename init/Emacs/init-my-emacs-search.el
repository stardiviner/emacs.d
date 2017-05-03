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
(require 'init-my-emacs-search-ack)
(require 'init-my-emacs-search-ag)
(require 'init-my-emacs-search-pt)
(require 'init-my-emacs-search-ripgrep)
(require 'init-my-emacs-search-finder)
(require 'init-my-emacs-search-engine)

(define-key my-search-prefix (kbd "s") 'ag)

;;; [ socyl ] -- The Emacs frontend for several search tools (ag, pt, sift, ripgrep, ...).

;; (use-package socyl
;;   :ensure t
;;   :bind (:map my-search-prefix
;;               ("s" . socyl-search-regexp))
;;   :config
;;   (setq socyl-backend 'ag)
;;   )


(provide 'init-my-emacs-search)

;;; init-my-emacs-search.el ends here
