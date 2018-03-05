;;; init-emacs-search-finder.el --- init for command find
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'find-prefix)
  (define-prefix-command 'find-prefix))

(define-key search-prefix (kbd "f") 'find-prefix)

;;; [ helm-fuzzy-find ] -- Find files using Fuzzy Search (fuzzy-find) with Helm.

(use-package helm-fuzzy-find
  :ensure t
  :ensure-system-package (ff . "cd ~/Code/Emacs/ff/ ; make && mv ff ~/bin/")
  :bind (("C-c C-/" . helm-fuzzy-find)
         :map find-prefix
         ("h" . helm-fuzzy-find))
  :config
  ;; (setq helm-fuzzy-find-keybind "")
  )


(provide 'init-emacs-search-finder)

;;; init-emacs-search-finder.el ends here