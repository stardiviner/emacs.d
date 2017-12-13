;;; init-my-emacs-search-finder.el --- init for command find
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'find-prefix)
  (define-prefix-command 'find-prefix))

(define-key search-prefix (kbd "f") 'find-prefix)

;;; [ helm-fuzzy-find ] -- Find files using Fuzzy Search (fuzzy-find) with Helm.

(use-package helm-fuzzy-find
  :ensure t
  :bind (("C-c C-/" . helm-fuzzy-find)
         :map find-prefix
         ("h" . helm-fuzzy-find))
  :config
  ;; (setq helm-fuzzy-find-keybind "")
  )


(provide 'init-my-emacs-search-finder)

;;; init-my-emacs-search-finder.el ends here
