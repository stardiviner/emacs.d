;;; init-my-emacs-search-ripgrep.el --- init for ripgrep
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
;;; [ ripgrep ] -- Emacs front-end for ripgrep, a command line search tool.

(use-package ripgrep
  :ensure t
  :bind (:map my-search-prefix
              ("s" . ripgrep-regexp))
  )


;;; ----------------------------------------------------------------------------

(provide 'init-my-emacs-search-ripgrep)

;;; init-my-emacs-search-ripgrep.el ends here
