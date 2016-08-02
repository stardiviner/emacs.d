;;; init-my-authoring.el --- init for Authoring & Writing.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ org-if ] -- Interactive Fiction Authoring System for Emacs and Org-Mode.

(use-package org-if
  :ensure t
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((org-if . t)))
  )


(provide 'init-my-authoring)

;;; init-my-authoring.el ends here
