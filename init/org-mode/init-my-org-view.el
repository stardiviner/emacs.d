;;; init-my-org-view.el --- init for Org View
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;; [ default view ]

(setq org-startup-folded t
      org-startup-truncated t
      ;; org-startup-with-beamer-mode nil
      ;; org-hide-block-startup t
      org-hide-emphasis-markers t
      )

(setq org-fontify-emphasized-text t
      org-fontify-quote-and-verse-blocks t
      org-fontify-done-headline t
      )

(setq org-ellipsis "  ")

;; include ' in org-verbatim face highlight.
(setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n,\"")
(org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

;; [ pretty entities ]

;; \pi will display as π
;; [C-c C-x \]
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

(use-package org-indent
  :init
  (setq org-startup-indented t
        org-hide-leading-stars t)
  :config
  ;; (set-face-attribute 'org-indent nil
  ;;                     :inherit 'org-hide
  ;;                     :foreground (face-background 'default)
  ;;                     )
  )

;;; [ org-numbers-overlay ]

;; (load "org-numbers-overlay")
;; (with-eval-after-load "org"
;;   (require 'org-numbers-overlay)
;;   (add-hook 'org-mode-hook #'org-numbers-overlay-mode))


(provide 'init-my-org-view)

;;; init-my-org-view.el ends here
