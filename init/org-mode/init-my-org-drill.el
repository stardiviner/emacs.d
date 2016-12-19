;;; init-my-org-drill.el --- init for Memory with Drill Sessions based on Org-mode.

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
;;; [ org-drill ] -- Begin an interactive "drill session" based on Org-mode.

(use-package org-drill
  :config
  ;; add org-drill topic property into default properties list.
  (add-to-list 'org-default-properties "DRILL_CARD_TYPE")

  (setq org-drill-use-visible-cloze-face-p t
        org-drill-hide-item-headings-p nil
        org-drill-save-buffers-after-drill-sessions-p nil
        ;; org-drill-spaced-repetition-algorithm 'sm2
        )
  )

;;; [ org-drill-table ] -- generate org-drill from org-mode tables.

(use-package org-drill-table
  :ensure t)



;;; ----------------------------------------------------------------------------

(provide 'init-my-org-drill)

;;; init-my-org-drill.el ends here
