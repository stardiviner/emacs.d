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

;;; [ auto-complete-rst ] -- auto-complete extension for ReST and Sphinx.

(use-package auto-complete-rst
  :ensure t
  :config
  (auto-complete-rst-init)
  (setq auto-complete-rst-other-sources
        '(ac-source-filename
          ac-source-abbrev
          ac-source-dictionary
          ac-source-yasnippet))
  )

;;; [ ox-rst ] -- reStructuredText export backend for Org-mode.

(use-package ox-rst
  :ensure t)

;;; [ sphinx-mode ] -- minor mode providing sphinx support.

(use-package sphinx-mode
  :ensure t
  :config
  (add-hook 'rst-mode-hook 'sphinx-mode)
  )

;;; ----------------------------------------------------------------------------

(provide 'init-my-prog-lang-reStructuredText)

;;; init-my-prog-lang-reStructuredText.el ends here
