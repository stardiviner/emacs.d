;;; init-org-view.el --- init for Org View
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; apply `variable-pitch-mode' on Org Mode.
(add-hook 'org-mode-hook 'variable-pitch-mode)

(setq org-ellipsis "  ")

;;; [ org-num ] -- Dynamic Headlines Numbering.

;; (use-package org-num
;;   :init (setq org-num-skip-footnotes t
;;               org-num-skip-unnumbered t)
;;   (add-hook 'org-mode-hook #'org-num-mode))

;;; [ org-bullets ]

(use-package org-bullets
  :ensure t
  :ensure all-the-icons
  :defer t
  :init (add-hook 'org-mode-hook #'org-bullets-mode)
  :config
  (with-eval-after-load 'all-the-icons
    (setq-default org-bullets-bullet-list
                  `(;; disk -> folder -> file style
                    ,(all-the-icons-faicon "hdd-o")
                    ;; ,(all-the-icons-faicon "cube")
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
                    ))))


(provide 'init-org-view)

;;; init-org-view.el ends here
