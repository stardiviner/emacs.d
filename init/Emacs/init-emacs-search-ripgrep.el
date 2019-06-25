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
  :after projectile
  :defer t
  :custom (rg-keymap-prefix nil)
  :preface (setq rg-keymap-prefix rg-prefix)
  ;; (setq rg-command-line-flags '("--debug"))
  :commands (rg rg-literal rg-dwim rg-dwim-current-dir rg-dwim-project-dir)
  :bind (:map search-prefix ("s" . rg)
              :map rg-prefix
              ("r" . rg-dwim)
              ("d" . rg-dwim-current-dir)
              ("p" . rg-dwim-project-dir)
              ("C-r" . rg)
              ("R" . rg-literal)
              :map projectile-command-map
              ("s r" . rg-project)
              ;; swap `projectile-ag' keybinding.
              ("s s" . rg-project)
              ("s a" . projectile-ag))
  :init (rg-enable-default-bindings)
  (setq rg-group-result t)
  (if (null rg-command-line-flags)
      (setq rg-command-line-flags '("-j 4"))
    (add-to-list 'rg-command-line-flags "-j 4"))
  :config (if (fboundp 'wgrep-rg-setup)
              (add-hook 'rg-mode-hook #'wgrep-rg-setup))
  (add-to-list 'display-buffer-alist
               '("^\\*rg\\*" (display-buffer-reuse-window display-buffer-below-selected))))

;;; [ helm-rg ]

(use-package helm-rg
  :ensure t
  :defer t
  :commands (helm-rg)
  :bind (:map rg-prefix ("h" . helm-rg)))


(provide 'init-emacs-search-ripgrep)

;;; init-emacs-search-ripgrep.el ends here
