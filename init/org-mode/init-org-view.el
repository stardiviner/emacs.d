;;; init-org-view.el --- init for Org View
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; apply `variable-pitch-mode' on Org Mode.
;; (add-hook 'org-mode-hook 'variable-pitch-mode)

;; (setq org-ellipsis "  ")

(setq org-pretty-entities t ; for `org-toggle-pretty-entities'
      org-use-sub-superscripts '{})

(setq org-fontify-quote-and-verse-blocks t)

;;; indent

;; (setq org-adapt-indentation 'headline-data)
;; (setq org-startup-indented t) ; enable `org-indent-mode' at startup.

;;; [ org-num ] -- Dynamic Headlines Numbering.

(setq org-startup-numeroted nil ; use "#+startup: num" file locally instead.
      org-num-skip-footnotes t
      org-num-skip-unnumbered t)

;;; [ org-superstar ]

(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :config
  (with-eval-after-load 'all-the-icons
    ;; (setq org-superstar-headline-bullets-list
    ;;       '("⓪" "①" "②" "③" "④" "⑤" "⑥" "⑦" "⑧" "⑨" "⑩"
    ;;         "⑪" "⑫" "⑬" "⑭" "⑮" "⑯" "⑰" "⑱" "⑲" "⑳"))
    (setq-default org-superstar-headline-bullets-list
                  `(;; disk -> folder -> file style
                    ;; folder -> open folder -> inbox -> book -> text -> file -> floppy -> bookmark
                    ;;===========================================================
                    ;; ,(all-the-icons-faicon "hdd-o")
                    ,(all-the-icons-faicon "folder-o")
                    ,(all-the-icons-faicon "folder-open-o")
                    ,(all-the-icons-faicon "inbox")
                    ;; ,(all-the-icons-faicon "book")
                    ,(all-the-icons-faicon "file-text-o")
                    ,(all-the-icons-faicon "file-o")
                    ,(all-the-icons-faicon "floppy-o")
                    ,(all-the-icons-faicon "bookmark")
                    ))))

;;; [ shrface ] -- It is a shr faces package. Org Like faces for shr, dash-docs, eww, nov.el, mu4e and more!

(use-package shrface
  :ensure t
  :defer t
  :config
  ;; Enable source codes highlight.
  (use-package shr-tag-pre-highlight
    :ensure t
    :defer t
    :config
    (add-to-list 'shr-external-rendering-functions '(pre . shr-tag-pre-highlight))))

;;===============================================================================
;;; NOTE: This setting will cause read-only-mode keybinding overriding.
(setq view-read-only t) ; enable view mode after enabled `read-only-mode' [C-x C-q].


(provide 'init-org-view)

;;; init-org-view.el ends here
