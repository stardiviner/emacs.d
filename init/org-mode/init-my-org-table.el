;;; init-my-org-table.el --- init for Org Table
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(setq org-enable-table-editor t)

;; (set-face-attribute 'org-column nil
;;                     :height (face-attribute 'default :height)
;;                     :family (face-attribute 'default :family))


;;; [ org-table-sticky-header ] -- Sticky header for org-mode tables.

(use-package org-table-sticky-header
  :ensure t
  :init
  (add-hook 'org-mode-hook 'org-table-sticky-header-mode)
  )

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

;;; [ otama ] -- Simple org-table based database, intended to be a light version of BBDB and helm-friendly.

;; (use-package otama
;;   :ensure t
;;   :defer t
;;   :init
;;   (setq otama-database-file-name
;;         (concat (getenv "HOME") "/Org" "/otama/otama.org"))
;;
;;   (define-key Org-prefix (kbd "D") 'otama-helm)
;;   )



(provide 'init-my-org-table)

;;; init-my-org-table.el ends here
