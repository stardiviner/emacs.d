;;; init-org-table.el --- init for Org Table
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(add-to-list 'display-buffer-alist
             '("\^\\*Org Table Edit Field\\*" (display-buffer-reuse-window display-buffer-below-selected)))

;;; [ org-table-sticky-header ] -- Sticky header for org-mode tables.

;; (use-package org-table-sticky-header
;;   :ensure t
;;   :init (add-hook 'org-mode-hook 'org-table-sticky-header-mode))

;; [ org-plot ] -- Plotting Tables in Org-mode.

(require 'org-plot)

;;; Org Table translator functions.
(add-to-list 'org-default-properties "ORGTBL") ; for Org-mode Table translator functions.

;; define a keybinding for org table translator functions
(define-key org-mode-map (kbd "C-c \" i") 'orgtbl-insert-radio-table)
(define-key org-mode-map (kbd "C-c \" s") 'orgtbl-send-table)

;;; [ orgtbl-ascii-plot ] -- ascii-art bar plots in org-mode tables.

;; #+TBLFM: $4='(orgtbl-ascii-draw plot min max)
;;
;; - #+TBLFM: $4='(orgtbl-ascii-draw $2 0 10)

(use-package orgtbl-ascii-plot
  :ensure t
  :defer t)

;;; [ orgtbl-aggregate ] -- create an aggregated Org table from another one.

(use-package orgtbl-aggregate
  :ensure t
  :commands (org-insert-dblock org-insert-dblock:aggregate)
  :init
  ;; add `orgtbl-aggregate' dynamic blocks into list.
  (org-dynamic-block-define "aggregate" 'org-insert-dblock:aggregate)
  (org-dynamic-block-define "invoice" 'org-insert-dblock:invoice)
  (org-dynamic-block-define "join" 'org-insert-dblock:join)
  (org-dynamic-block-define "org-gantt" 'org-insert-dblock:org-gantt)
  (org-dynamic-block-define "propview" 'org-insert-dblock:propview)
  (org-dynamic-block-define "transpose" 'org-insert-dblock:transpose))

;;; [ orgtbl-join ] -- join two Org-mode tables.

(use-package orgtbl-join
  :ensure t
  :defer t)

;;; import .xlsx, .csv file into Org.
(defun org-table-import-xlsx-to-csv-org ()
  (interactive)
  (let* ((source-file  (file-name-sans-extension (buffer-file-name (current-buffer))))
         (xlsx-file (concat source-file ".xlsx"))
         (csv-file (concat source-file ".csv")))
    (org-odt-convert xlsx-file "csv")
    (org-table-import csv-file  nil)))

(defun org-table-import-xlsx-file-to-csv-org (file)
  "Import .xlsx, .csv `FILE' into Org."
  (interactive "f")
  (let* ((source-file  (file-name-sans-extension (buffer-file-name (current-buffer))))
         (xlsx-file (concat source-file ".xlsx"))
         (csv-file (concat source-file ".csv")))
    (org-odt-convert file "csv")
    (org-table-import csv-file  nil)))

;;; [ org-transform-tree-table ] -- Transform an org-mode outline and its properties to a table format (org-table, CSV).

(use-package org-transform-tree-table
  :ensure t
  :defer t)

;;; [ mysql-to-org-mode ] -- Minor mode for emacs to output the results of mysql queries to org tables.

(use-package mysql-to-org
  :ensure t
  :defer t)



(provide 'init-org-table)

;;; init-org-table.el ends here
