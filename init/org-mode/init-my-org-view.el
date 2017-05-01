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

;; [ org-bullets ]

;; (use-package org-bullets
;;   :ensure t
;;   :config
;;   (setq-default org-bullets-bullet-list
;;                 ;; '("◉" "❀" "✿" "✪" "☯" "✜" "✩" "✡" "◌" "◉" "⍟" "☢")
;;                 ;; '("①" "②" "③" "④" "⑤" "⑥" "⑦" "⑧" "⑨" "⑩")
;;                 '("Ⅰ" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ" "Ⅵ" "Ⅶ" "Ⅷ" "Ⅸ" "Ⅹ" "Ⅺ" "Ⅻ")
;;                 ;; '("⊢" "⋮" "⋱" " ")
;;                 )
;;
;;   (defface org-bullets-face
;;     '((t (:inherit nil)))
;;     "My custom face for org-bullets."
;;     :group 'org-faces)
;;
;;   ;; (set-face-attribute 'org-bullets-face nil
;;   ;;                     :family "DejaVu Sans"
;;   ;;                     :height 130 :weight 'bold
;;   ;;                     )
;;
;;   (setq org-bullets-face-name 'org-bullets-face)
;;
;;   (add-hook 'org-mode-hook #'org-bullets-mode)
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Complete path numbering of org-mode headlines and plain lists.
(require 'cl)
(require 'dash)

(defun overlay-numbered-headings ()
  "Put numbered overlays on the headings."
  (interactive)
  (loop for (p lv) in (let ((counters (copy-list '(0 0 0 0 0 0 0 0 0 0)))
                            (current-level 1)
                            last-level)
                        (mapcar (lambda (x)
                                  (list (car x)
                                        ;; trim trailing zeros
                                        (let ((v (nth 1 x)))
                                          (while (= 0 (car (last v)))
                                            (setq v (butlast v)))
                                          v)))
                                (org-map-entries
                                 (lambda ()
                                   (let* ((hl (org-element-context))
                                          (level (org-element-property :level hl)))
                                     (setq last-level current-level
                                           current-level level)
                                     (cond
                                      ;; no level change or increase, increment level counter
                                      ((or (= last-level current-level)
                                           (> current-level last-level))
                                       (incf (nth current-level counters)))

                                      ;; decrease in level
                                      (t
                                       (loop for i from (+ 1 current-level) below (length counters)
                                             do
                                             (setf (nth i counters) 0))
                                       (incf (nth current-level counters))))

                                     (list (point) (-slice counters 1)))))))
        do
        (let ((ov (make-overlay p (+ 1 p))))
          (overlay-put ov 'display (concat (mapconcat 'number-to-string lv ".") ". "))
          (overlay-put ov 'numbered-heading t))))

(define-minor-mode numbered-org-mode
  "Minor mode to number org headings."
  :init-value nil
  (if numbered-org-mode
      (overlay-numbered-headings)
    (ov-clear 'numbered-heading)))

;; (set-face-attribute 'org-hide nil
;;                     :foreground "white")
;; (set-face-attribute 'org-indent nil
;;                     :background (face-background 'default)
;;                     :foreground (face-background 'default)
;;                     )

;; (add-hook 'org-mode-hook #'numbered-org-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
