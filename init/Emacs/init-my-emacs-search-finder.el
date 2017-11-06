;;; init-my-emacs-search-finder.el --- init for command find
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'find-prefix)
  (define-prefix-command 'find-prefix))

(define-key my-search-prefix (kbd "f") 'find-prefix)

;;; [ helm-fuzzy-find ] -- Find files using Fuzzy Search (fuzzy-find) with Helm.

(use-package helm-fuzzy-find
  :ensure t
  :bind (("C-c C-/" . helm-fuzzy-find)
         :map find-prefix
         ("h" . helm-fuzzy-find))
  :config
  ;; (setq helm-fuzzy-find-keybind "")
  )

;;; [ Deft ] -- quickly browsing, filtering, and editing directories of plain text notes.

(use-package deft
  :ensure t
  :bind ("<f7>" . deft)
  :commands (deft)
  :config
  (setq deft-extensions '("org" "tex" "txt" "md" "markdown"))
  (with-eval-after-load 'org
    (setq deft-directory (concat org-directory "/Projects")))
  (setq deft-recursive t)
  (setq deft-use-filename-as-title t)
  ;; (setq deft-strip-summary-regexp)
  )


(provide 'init-my-emacs-search-finder)

;;; init-my-emacs-search-finder.el ends here
