;;; init-prog-complete.el --- init for Programming Completion
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


;;; [ pcomplete ] --- Programmable, Context-Sensitive Completion Library

(use-package pcomplete
  :defer t
  :init (setq pcomplete-ignore-case t))


(require 'init-auto-complete)
(require 'init-company-mode)
;; (require 'init-jetbrains)


(provide 'init-prog-complete)

;;; init-prog-complete.el ends here
