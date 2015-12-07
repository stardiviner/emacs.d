;;; init-my-org-presentation.el --- init for Org presentation
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ org-present ]

(use-package org-present
  :config
  ;; Precise behaviour of org-present during start and quit is controlled from
  ;; hooks. The following will enlarge text, show images, hide the cursor and
  ;; make the buffer read-only:
  (add-hook 'org-present-mode-hook
            '(lambda ()
               (org-present-big)
               (org-display-inline-images)
               (org-present-hide-cursor)
               (org-present-read-only)

               ;; bind [SPACE] to navigate to next slice.
               (define-key org-present-mode-keymap (kbd "SPC") 'org-present-next)
               ))
  (add-hook 'org-present-mode-quit-hook
            '(lambda ()
               (org-present-small)
               (org-remove-inline-images)
               (org-present-show-cursor)
               (org-present-read-write)))

  (defun my-org-present ()
    "Toggle org-present."
    (interactive)
    (if org-present-mode
        (org-present-quit)
      (org-present)))

  (define-key my-org-prefix (kbd "p") 'my-org-present)
  )


;;; [ org-tree-slide ] -- A presentation tool for org-mode based on the visibility of outline trees.

(use-package org-tree-slide
  :config
  (setq org-tree-slide-header t
        org-tree-slide-cursor-init t
        org-tree-slide-skip-done nil
        org-tree-slide-slide-in-effect t
        org-tree-slide-heading-emphasis t
        org-tree-slide-modeline-display 'outside
        org-tree-slide-fold-subtrees-skipped t)
  
  ;; profiles
  ;; (org-tree-slide-simple-profile)
  (org-tree-slide-presentation-profile)
  ;; (org-tree-slide-narrowing-control-profile)

  (if (featurep 'org-tree-slide-mode)
      (define-key my-org-prefix (kbd "p") 'org-tree-slide-mode)
    ;; (global-set-key (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle)
    )
  )


(provide 'init-my-org-presentation)

;;; init-my-org-presentation.el ends here
