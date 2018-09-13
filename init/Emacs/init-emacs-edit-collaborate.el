;;; init-emacs-edit-collaborate.el --- init Emacs for collaboration editing.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ floobits ] -- Floobits client for Emacs.

(use-package floobits
  :ensure t
  :defer t
  :config
  (add-to-list 'display-buffer-alist
               '("\\*Floobits\\*" . (display-buffer-reuse-window display-buffer-below-selected))))




(provide 'init-emacs-edit-collaborate)

;;; init-emacs-edit-collaborate.el ends here
