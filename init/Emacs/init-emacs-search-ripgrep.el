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
  :after projectile
  :preface
  (add-to-list 'exec-path (or (concat (getenv "CARGO_HOME") "/bin") (expand-file-name "~/.cargo/bin/")))
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
  :custom (;; (rg-command-line-flags '("--debug"))
           (rg-keymap-prefix nil)
           (rg-group-result t)
           (rg-command-line-flags '("-j 4")))
  :init
  (rg-enable-default-bindings)
  (add-to-list 'display-buffer-alist '("^\\*rg\\*" . (display-buffer-below-selected)))
  :config
  (if (fboundp 'wgrep-rg-setup) (add-hook 'rg-mode-hook #'wgrep-rg-setup))

  ;; automatically "reveal context" when opening matches in org buffers.
  (defun rg-reveal-org ()
    "Call`org-reveal' if current buffer is an `org-mode' buffer."
    (when (derived-mode-p 'org-mode) (org-reveal)))
  (defun rg-next-error-reveal-org ()
    (add-hook 'next-error-hook #'rg-reveal-org nil 'local))
  (add-hook 'rg-mode-hook #'rg-next-error-reveal-org))

;;; [ helm-rg ]

(use-package helm-rg
  :ensure t
  :defer t
  :commands (helm-rg)
  :bind (:map rg-prefix ("h" . helm-rg)))


(provide 'init-emacs-search-ripgrep)

;;; init-emacs-search-ripgrep.el ends here
