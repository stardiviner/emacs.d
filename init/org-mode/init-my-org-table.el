;;; init-my-org-table.el --- init for Org Table
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(setq org-enable-table-editor t)



;;; [ orgtbl-ascii-plot ] -- ascii-art bar plots in org-mode tables.

;; #+TBLFM: $4='(orgtbl-ascii-draw plot min max)
;;
;; - #+TBLFM: $4='(orgtbl-ascii-draw $2 0 10)

(use-package orgtbl-ascii-plot
  :ensure t
  :defer t)

;;; [ org-table-sticky-header ] -- Sticky header for org-mode tables.

(use-package org-table-sticky-header
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-table-sticky-header-mode)
  )


(provide 'init-my-org-table)

;;; init-my-org-table.el ends here
