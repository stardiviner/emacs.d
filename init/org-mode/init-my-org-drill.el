;;; init-my-org-drill.el --- init for Memory with Drill Sessions based on Org-mode.

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
;;; [ org-drill ] -- Begin an interactive "drill session" based on Org-mode.

(use-package org-drill
  :config
  ;; add org-drill topic property into default properties list.
  (add-to-list 'org-default-properties "DRILL_CARD_TYPE")

  (setq org-drill-use-visible-cloze-face-p nil ; t will caused [] invalid headline fontify.
        org-drill-hide-item-headings-p nil
        org-drill-save-buffers-after-drill-sessions-p nil
        ;; org-drill-spaced-repetition-algorithm 'sm2
        )
  )

;;; [ org-drill-table ] -- generate org-drill from org-mode tables.

(use-package org-drill-table
  :ensure t)


;;; [ pamparam ] -- Simple and fast flashcards for Emacs.

(use-package pamparam
  :quelpa (pamparam :fetcher github :repo "abo-abo/pamparam")
  )

;;; ----------------------------------------------------------------------------

(provide 'init-my-org-drill)

;;; init-my-org-drill.el ends here
