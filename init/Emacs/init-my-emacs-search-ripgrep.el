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
  :config
  ;; (setq ripgrep-arguments '())
  )

;;; [ rg ] -- Use ripgrep (grep and ag replacement) like rgrep.

(use-package rg
  :ensure t
  :bind (:map my-search-prefix
              ("r" . rg))
  :config
  (setq rg-command-line-flags '()
        rg-group-result t
        rg-show-columns t)
  )

;;; [ projectile-ripgrep ] -- front-end for ripgrep, a command line search tool.

(use-package projectile-ripgrep
  :ensure t
  :config
  (define-key my-search-prefix (kbd "C-r") 'projectile-ripgrep)
  )

;;; ----------------------------------------------------------------------------

(provide 'init-my-emacs-search-ripgrep)

;;; init-my-emacs-search-ripgrep.el ends here
