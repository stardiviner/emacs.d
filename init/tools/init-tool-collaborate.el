;;; init-tool-collaborate.el --- init Emacs for collaboration
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ floobits ] -- Floobits client for Emacs.

(use-package floobits
  :ensure t
  :defer t
  :config
  (add-to-list 'display-buffer-alist
               '("\\*Floobits\\*" . (display-buffer-below-selected))))




(provide 'init-tool-collaborate)

;;; init-tool-collaborate.el ends here
