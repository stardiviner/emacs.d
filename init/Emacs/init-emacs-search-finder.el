;;; init-emacs-search-finder.el --- init for command find
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'find-prefix)
  (define-prefix-command 'find-prefix))

(define-key search-prefix (kbd "f") 'find-prefix)

(define-key find-prefix (kbd "h") 'helm-find)

;;; [ find-dired ]

(use-package find-dired
  :commands (find-dired find-name-dired find-grep-dired)
  :bind (:map find-prefix
              ("f" . find-dired)
              ("n" . find-name-dired)
              ("g" . find-grep-dired))
  :init
  (add-to-list 'display-buffer-alist
               '("\\*Find\\*" . (display-buffer-reuse-window display-buffer-below-selected)))
  )


(provide 'init-emacs-search-finder)

;;; init-emacs-search-finder.el ends here
