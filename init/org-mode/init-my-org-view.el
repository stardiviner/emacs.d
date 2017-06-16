;;; init-my-org-view.el --- init for Org View
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;; [ default view ]

(setq org-startup-folded t
      org-startup-truncated t
      ;; org-startup-with-beamer-mode nil
      ;; org-hide-block-startup t
      org-hide-leading-stars t
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
      org-pretty-entities-include-sub-superscripts t)

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

;; (use-package org-indent
;;   :init
;;   (setq org-startup-indented t)
;;   :config
;;   (setq org-indent-mode-turns-on-hiding-stars t
;;         org-indent-indentation-per-level 1
;;         )
;;
;;   ;; (setq org-indent-boundary-char ?\s) ; ?\s, ?|
;;
;;   (set-face-attribute 'org-indent nil
;;                       :inherit 'org-hide
;;                       :foreground (face-background 'default)
;;                       )
;;   )


;; [ org-index ] -- fast search for selected org nodes and things outside of Org.
(use-package org-index
  :init
  (setq org-index-dispatch-key (kbd "C-c o i"))
  :config
  ;; (setq org-index-id)
  )

;; [ org-bullets ]

(use-package org-bullets
  :ensure t
  :config
  (setq-default org-bullets-bullet-list
                '("Ⅰ" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ" "Ⅵ" "Ⅶ" "Ⅷ" "Ⅸ" "Ⅹ" "Ⅺ" "Ⅻ")
                )

  (defface org-bullets-face
    '((t (:inherit 'org-level-1 :height 1.2)))
    "My custom face for org-bullets."
    :group 'org-faces)
  (set-face-attribute 'org-bullets-face nil
                      :family "Monospace"
                      :height 120 :weight 'normal
                      )
  (setq org-bullets-face-name 'org-bullets-face)

  (add-hook 'org-mode-hook #'org-bullets-mode)
  )

;;; [ org-numbers-overlay ]

;; (load "org-numbers-overlay")
(require 'org-numbers-overlay)
(add-hook 'org-mode-hook #'org-numbers-overlay-mode)


(require 'org-list)

(setq org-list-demote-modify-bullet
      '(("+" . "-")
        ;; ("-" . "+")
        ("*" . "-")
        ))


(provide 'init-my-org-view)

;;; init-my-org-view.el ends here
