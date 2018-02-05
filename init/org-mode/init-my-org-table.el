;;; init-my-org-table.el --- init for Org Table
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(setq org-enable-table-editor t)

(add-to-list 'display-buffer-alist
             '("\^\\*Org Table Edit Field\\*" (display-buffer-below-selected)))

;; (set-face-attribute 'org-column nil
;;                     :height (face-attribute 'default :height)
;;                     :family (face-attribute 'default :family))


;;; [ org-table-sticky-header ] -- Sticky header for org-mode tables.

(use-package org-table-sticky-header
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-table-sticky-header-mode))

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
  :defer t)

;;; import .xlsx, .csv file into Org.
(defun org-table-import-xlsx-file-to-csv-org (file)
  "Import .xlsx, .csv `FILE' into Org."
  (interactive "f")
  (let* ((source-file  (file-name-sans-extension (buffer-file-name (current-buffer))))
         (xlsx-file (concat source-file ".xlsx"))
         (csv-file (concat source-file ".csv")))
    (org-odt-convert file "csv")
    (org-table-import csv-file  nil)))



(provide 'init-my-org-table)

;;; init-my-org-table.el ends here
