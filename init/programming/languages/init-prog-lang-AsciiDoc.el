;;; init-prog-lang-AsciiDoc.el --- init for AsciiDoc document.

;;; Time-stamp: <2019-02-26 12:02:30 stardiviner>

;;; Commentary:



;;; Code:

;;; [ adoc-mode ] -- a major-mode for editing AsciiDoc files in Emacs.

(use-package adoc-mode
  :ensure t
  :mode (("\\.adoc\\'" . adoc-mode)))



(provide 'init-prog-lang-AsciiDoc)

;;; init-prog-lang-AsciiDoc.el ends here
