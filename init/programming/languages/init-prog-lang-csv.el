;;; init-prog-lang-csv.el --- init file for CSV -*- lexical-binding: t; -*-

;;; Commentary:



;;; Code:

;;; [ csv-mode ] -- major mode for editing comma/char separated values.

(use-package csv-mode
  :ensure t
  :defer t
  :hook ((csv-mode . csv-align-mode)
         (csv-mode . csv-header-line)))




(provide 'init-prog-lang-csv)

;;; init-prog-lang-csv.el ends here
