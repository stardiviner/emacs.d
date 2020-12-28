;;; init-org-view.el --- init for Org View
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; set Org headlines font
(let* ((variable-tuple
        (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
              ((x-list-fonts "DejaVu Sans Mono") '(:font "DejaVu Sans Mono"))
              ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
              ((x-list-fonts "Fira Sans") '(:font "Fira Sans"))
              ((x-list-fonts "Hack") '(:font "Hack"))
              ((x-list-fonts "Monaco") '(:font "Monaco"))
              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       ;; (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(;; :inherit default
                             :weight bold
                             ;; :foreground ,base-font-color
                             )))

  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline ,@variable-tuple))))
   `(org-level-7 ((t (,@headline ,@variable-tuple))))
   `(org-level-6 ((t (,@headline ,@variable-tuple))))
   `(org-level-5 ((t (,@headline ,@variable-tuple))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
   `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0))))))


;;; apply `variable-pitch-mode' on Org Mode.
;; (add-hook 'org-mode-hook 'variable-pitch-mode)

;; (setq org-ellipsis "  ")

(setq org-pretty-entities t             ; for `org-toggle-pretty-entities'
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
  :custom ((org-hide-leading-stars nil)        ; --| hide headline
           (org-superstar-leading-bullet ?\s)) ; --| leading stars
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

;;; [ org-sticky-header ] -- Show off-screen Org heading at top of window.

;; (use-package org-sticky-header
;;   :ensure t
;;   :hook (org-mode . org-sticky-header-mode))

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
