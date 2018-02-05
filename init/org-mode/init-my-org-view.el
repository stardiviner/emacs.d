;;; init-my-org-view.el --- init for Org View
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;; [ default view ]

(setq org-startup-folded t)

(setq org-hide-emphasis-markers t
      org-fontify-emphasized-text t
      org-fontify-quote-and-verse-blocks t
      org-fontify-done-headline t)

(setq org-ellipsis "  ")

;; include ' in org-verbatim face highlight.
(setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n,\"")
(org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

;; [ pretty entities ]

;; \pi will display as π
(setq org-pretty-entities t
      org-pretty-entities-include-sub-superscripts nil)

(setq org-script-display
      '(((raise -0.3)
         (height 0.7)
         (:foreground "yellow"))
        ((raise 0.3)
         (height 0.7)
         (:foreground "yellow"))
        ((raise -0.5))
        ((raise 0.5))
        )
      )

;; [ indent ]

(require 'org-indent)
(setq org-startup-indented t
      org-hide-leading-stars t)
;; (set-face-attribute 'org-indent nil
;;                     :inherit 'org-hide
;;                     :foreground (face-background 'default)
;;                     )


(provide 'init-my-org-view)

;;; init-my-org-view.el ends here
