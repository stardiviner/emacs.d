;;; init-my-org-view.el --- init for Org View
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;; [ default view ]

(setq org-startup-folded t
      org-startup-indented t
      org-startup-truncated t
      ;; org-startup-with-beamer-mode nil
      ;; org-hide-block-startup t
      org-hide-leading-stars t
      org-indent-mode-turns-on-hiding-stars t
      org-indent-indentation-per-level 1
      org-hide-emphasis-markers t
      )

(setq org-fontify-emphasized-text t
      org-fontify-quote-and-verse-blocks t
      org-fontify-done-headline t
      )

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

(use-package org-indent
  :init
  (add-hook 'org-mode-hook 'org-indent-mode)
  :config
  (set-face-attribute 'org-indent nil
                      :inherit 'org-hide
                      :foreground (face-background 'default)
                      )
  )

;; [ org-bullets ]

(use-package org-bullets
  :ensure t
  :config
  (setq-default org-bullets-bullet-list
                ;; '("◉" "❀" "✿" "✪" "☯" "✜" "✩" "✡" "◌" "◉" "⍟" "☢")
                ;; '("①" "②" "③" "④" "⑤" "⑥" "⑦" "⑧" "⑨" "⑩")
                '("Ⅰ" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ" "Ⅵ" "Ⅶ" "Ⅷ" "Ⅸ" "Ⅹ" "Ⅺ" "Ⅻ")
                ;; '("⊢" "⋮" "⋱" " ")
                )

  (defface org-bullets-face
    '((t (:inherit nil)))
    "My custom face for org-bullets."
    :group 'org-faces)

  (set-face-attribute 'org-bullets-face nil
                      :family "DejaVu Sans"
                      :height 130 :weight 'bold
                      )

  (setq org-bullets-face-name 'org-bullets-face)

  (add-hook 'org-mode-hook #'org-bullets-mode)
  )


(require 'org-list)

(setq org-list-demote-modify-bullet
      '(("+" . "-")
        ;; ("-" . "+")
        ("*" . "-")
        ))


;;; [ org-beautify-theme ] -- A sub-theme to make org-mode more beautiful.

;; (require 'org-beautify-theme)


(provide 'init-my-org-view)

;;; init-my-org-view.el ends here
