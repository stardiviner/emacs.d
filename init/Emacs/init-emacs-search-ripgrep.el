;;; init-emacs-search-ripgrep.el --- init for ripgrep
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ ripgrep ] -- Emacs front-end for ripgrep, a command line search tool.

(use-package ripgrep
  :ensure t
  :ensure-system-package (rg . "sudo pacman -S --noconfirm ripgrep")
  :bind (:map search-prefix
              ("r" . ripgrep-regexp))
  :config
  ;; (setq ripgrep-arguments '())
  )

;;; [ projectile-ripgrep ] -- front-end for ripgrep, a command line search tool.

(use-package projectile-ripgrep
  :ensure t
  :config
  (define-key search-prefix (kbd "C-r") 'projectile-ripgrep)
  )


(provide 'init-emacs-search-ripgrep)

;;; init-emacs-search-ripgrep.el ends here
