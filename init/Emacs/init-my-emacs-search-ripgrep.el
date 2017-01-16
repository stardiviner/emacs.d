;;; init-my-emacs-search-ripgrep.el --- init for ripgrep
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
;;; [ ripgrep ] -- Emacs front-end for ripgrep, a command line search tool.

(use-package ripgrep
  :ensure t
  :bind (:map my-search-prefix
              ("M-r" . ripgrep-regexp))
  )

;;; [ rg ] -- Use ripgrep (grep and ag replacement) like rgrep.

(use-package rg
  :ensure t
  :bind (:map my-search-prefix
              ("r" . rg))
  )

;;; ----------------------------------------------------------------------------

(provide 'init-my-emacs-search-ripgrep)

;;; init-my-emacs-search-ripgrep.el ends here
