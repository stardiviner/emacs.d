;;; init-prog-lang-AsciiDoc.el --- init for AsciiDoc document.

;;; Commentary:



;;; Code:

;;; [ adoc-mode ] -- a major-mode for editing AsciiDoc files in Emacs.

(use-package adoc-mode
  :ensure t
  :mode (("\\.adoc\\'" . adoc-mode)))



(provide 'init-prog-lang-AsciiDoc)

;;; init-prog-lang-AsciiDoc.el ends here
