;;; init-my-prog-lang-reStructuredText.el --- init for reStructuredText
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
;;; [ rst ] -- Mode for viewing and editing reStructuredText-documents.

(use-package rst
  :ensure t
  :config
  (add-hook 'rst-adjust-hook 'rst-toc-update)
  )

;;; [ ox-rst ] -- reStructuredText export backend for Org-mode.

(use-package ox-rst
  :ensure t)

;;; ----------------------------------------------------------------------------

(provide 'init-my-prog-lang-reStructuredText)

;;; init-my-prog-lang-reStructuredText.el ends here
