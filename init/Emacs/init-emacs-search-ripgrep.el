;;; init-emacs-search-ripgrep.el --- init for ripgrep
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'rg-prefix)
  (define-prefix-command 'rg-prefix))
(define-key search-prefix (kbd "r") 'rg-prefix)

;;; [ rg ] -- Use ripgrep (grep and ag replacement) like rgrep.

(use-package rg
  :ensure t
  :defer t
  :custom (rg-keymap-prefix nil)
  :preface (setq rg-keymap-prefix rg-prefix)
  :bind (:map rg-prefix
              ("r" . rg-dwim)
              ("R" . rg-dwim-current-directory)
              ("C-r" . rg)
              :map projectile-command-map
              ("s r" . rg-project)
              ;; swap `projectile-ag' keybinding.
              ("s s" . rg-project)
              ("s a" . projectile-ag)
              )
  :init
  (rg-enable-default-bindings)
  (if (fboundp 'wgrep-ag-setup)
      (add-hook 'rg-mode-hook #'wgrep-ag-setup))
  :config
  (setq rg-command-line-flags '()
        rg-group-result t
        rg-show-columns t)
  (add-to-list 'display-buffer-alist
               '("^\\*rg\\*" (display-buffer-below-selected)))
  )


(provide 'init-emacs-search-ripgrep)

;;; init-emacs-search-ripgrep.el ends here
