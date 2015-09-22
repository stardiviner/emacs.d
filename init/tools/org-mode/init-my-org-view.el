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
      org-indent-indentation-per-level 2
      org-hide-emphasis-markers t
      )

(setq org-fontify-emphasized-text t
      org-fontify-quote-and-verse-blocks t
      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      )

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
  :config
  (set-face-attribute 'org-indent nil
                      :foreground nil :background nil
                      )
  )

;; [ org-bullets ]

(require 'org-bullets nil t)

(setq-default org-bullets-bullet-list
              ;; '("◉")
              '("✩" "✡" "✪" "☯" "✜" "◌" "◉" "⍟" "❀" "✿" "☢")
              ;; '("Ⅰ" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ" "Ⅵ" "Ⅶ" "Ⅷ" "Ⅸ" "Ⅹ" "Ⅺ" "Ⅻ")
              ;; '("❶" "❷" "❸" "❹" "❺" "❻" "❼" "❽" "❾" "❿")
              ;; '("①" "②" "③" "④" "⑤" "⑥" "⑦" "⑧" "⑨" "⑩")
              ;; '("㊀" "㊁" "㊂" "㊃" "㊄" "㊅" "㊆" "㊇" "㊈" "㊉")
              )

(defface org-bullets-face
  '((t (:inherit nil
                 :foreground "olive drab" :background nil
                 ;; :box '(:color "dark slate gray" :line-width 2)
                 )))
  "My custom face for org-bullets."
  :group 'org-faces)
(setq org-bullets-face-name 'org-bullets-face)

(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode 1)
            (set-face-attribute 'org-bullets-face nil
                                :background nil
                                :weight 'bold :height 150
                                :foreground "cyan"
                                ;; :box '(:color "dark slate gray" :line-width -1)
                                )
            ))



(provide 'init-my-org-view)

;;; init-my-org-view.el ends here
