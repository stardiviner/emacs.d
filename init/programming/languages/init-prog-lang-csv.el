;;; init-prog-lang-csv.el --- init file for CSV -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-06-26 17:52:44 stardiviner>

;;; Commentary:



;;; Code:

;;; [ csv-mode ] -- major mode for editing comma/char separated values.

(use-package csv-mode
  :ensure t
  :config
  (defun my/csv-align-setup ()
    (csv-align-mode)
    (csv-header-line))
  (add-hook 'csv-mode-hook #'my/csv-align-setup))




(provide 'init-prog-lang-csv)

;;; init-prog-lang-csv.el ends here
