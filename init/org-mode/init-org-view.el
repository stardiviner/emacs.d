;;; init-org-view.el --- init for Org View
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;; [ default view ]

(setq org-startup-truncated t) ; `truncate-lines'
(setq org-startup-folded t)

(setq org-hide-emphasis-markers t
      org-fontify-quote-and-verse-blocks t)

(setq org-ellipsis "  ")

;;; TODO: whether remove this extra setting which not supported by Org-mode built-in.
;; include ' in org-verbatim face highlight.
;; (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n,\"")
;; (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

;; [ pretty entities ]

;; \pi will display as π
(setq org-pretty-entities t
      org-pretty-entities-include-sub-superscripts t
      org-use-sub-superscripts "{}" ; force use {} to avoid _ confuse in Chinese and pretty-entities.
      )

(setq org-script-display
      '(((raise -0.3)
         (height 0.7)
         (:foreground "yellow"))
        ((raise 0.3)
         (height 0.7)
         (:foreground "yellow"))
        ((raise -0.5))
        ((raise 0.5))))

;; [ indent ]

;; (require 'org-indent)
;; (setq org-startup-indented t
;;       org-hide-leading-stars t)
;; (setq org-adapt-indentation nil)

;;; [ org-bullets ]

(use-package org-bullets
  :ensure t
  :ensure all-the-icons
  :after all-the-icons
  :init
  (setq-default org-bullets-bullet-list
                `(;; disk -> folder -> file style
                  ,(all-the-icons-faicon "hdd-o")
                  ,(all-the-icons-faicon "folder-open-o")
                  ,(all-the-icons-faicon "folder-o")
                  ,(all-the-icons-faicon "inbox")
                  ,(all-the-icons-faicon "book")
                  ,(all-the-icons-faicon "file-text-o")
                  ,(all-the-icons-faicon "file-o")
                  ,(all-the-icons-faicon "floppy-o")
                  ,(all-the-icons-faicon "header")
                  ,(all-the-icons-faicon "bookmark")
                  ,(all-the-icons-faicon "bookmark-o")
                  ,(all-the-icons-faicon "paperclip")
                  ,(all-the-icons-faicon "hashtag")
                  
                  ;; battery style.
                  ,(all-the-icons-faicon "battery-full")
                  ,(all-the-icons-faicon "battery-three-quarters")
                  ,(all-the-icons-faicon "battery-half")
                  ,(all-the-icons-faicon "battery-quarter")
                  ,(all-the-icons-faicon "battery-empty")
                  
                  ;; directions style
                  ;; ,(all-the-icons-faicon "chevron-circle-down")
                  ;; ,(all-the-icons-faicon "caret-square-o-right")
                  ;; ,(all-the-icons-faicon "arrow-circle-o-down")
                  ))
  (add-hook 'org-mode-hook #'org-bullets-mode)
  )


(provide 'init-org-view)

;;; init-org-view.el ends here
