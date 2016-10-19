;;; init-my-prog-bug-track-system.el --- init for Bug Tracking System
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'my-prog-bug-bts-map)
  (define-prefix-command 'my-prog-bug-bts-map))
(global-set-key (kbd "C-c b") 'my-prog-bug-bts-map)


;;; [ bts ] -- bug track system

(use-package pophint
  :ensure t)

(use-package bts
  :ensure t
  :defer t
  :init
  ;; Key Binding
  (define-key my-prog-bug-bts-map (kbd "b") 'bts:summary-open)
  (define-key my-prog-bug-bts-map (kbd "t") 'bts:ticket-new)
  (define-key my-prog-bug-bts-map (kbd "P") 'bts:project-new)
  (define-key my-prog-bug-bts-map (kbd "p") 'bts:project-update)
  (define-key my-prog-bug-bts-map (kbd "r") 'bts:project-remove)
  (define-key my-prog-bug-bts-map (kbd "R") 'bts:project-remove-all)
  (define-key my-prog-bug-bts-map (kbd "q") 'bts:query-new)
  (define-key my-prog-bug-bts-map (kbd "Q") 'bts:query-update)
  (define-key my-prog-bug-bts-map (kbd "d") 'bts:query-remove)
  (define-key my-prog-bug-bts-map (kbd "D") 'bts:query-remove-all)

  :config
  (setq bts:preferred-selection-method 'default
        bts:project-cache-file "~/.emacs.d/.bts/project"
        bts:query-cache-file "~/.emacs.d/.bts/query"
        bts:ticket-fetch-check-interval 3 ; seconds
        bts:ticket-multi-view-preferred t
        ;; widget label
        bts:widget-label-format " %s "
        bts:widget-label-prefix " "
        bts:widget-label-suffix " "
        bts:widget-require-mark "*"
        )


  ;; ;;; Faces
  ;; ;; summary
  ;; (set-face-attribute 'bts:summary-closed-ticket-face nil
  ;;                     )
  ;; (set-face-attribute 'bts:summary-condition-fetch-part-face nil
  ;;                     )
  ;; (set-face-attribute 'bts:summary-condition-grep-part-face nil
  ;;                     )
  ;; (set-face-attribute 'bts:summary-condition-marked-part-face nil
  ;;                     )
  ;; (set-face-attribute 'bts:summary-condition-query-part-face nil
  ;;                     )
  ;; (set-face-attribute 'bts:summary-condition-warn-part-face nil
  ;;                     )
  ;; (set-face-attribute 'bts:summary-ignored-ticket-face nil
  ;;                     )
  ;; (set-face-attribute 'bts:summary-mark-face nil
  ;;                     )
  ;; ;; ticket
  ;; (set-face-attribute 'bts:ticket-regist-message-failed-face nil
  ;;                     )
  ;; (set-face-attribute 'bts:ticket-regist-message-skipped-face nil
  ;;                     )
  ;; (set-face-attribute 'bts:ticket-regist-message-succeed-face nil
  ;;                     )
  ;; ;; widget
  ;; (set-face-attribute 'bts:widget-button-face nil
  ;;                     )
  ;; (set-face-attribute 'bts:widget-button-pressed-face nil
  ;;                     )
  ;; (set-face-attribute 'bts:widget-const-face nil
  ;;                     )
  ;; (set-face-attribute 'bts:widget-documentation-face nil
  ;;                     )
  ;; (set-face-attribute 'bts:widget-label-face nil
  ;;                     )
  ;; (set-face-attribute 'bts:widget-link-face nil
  ;;                     )
  ;; (set-face-attribute 'bts:widget-mouse-face nil
  ;;                     )
  ;; (set-face-attribute 'bts:widget-require-face nil
  ;;                     )
  ;; (set-face-attribute 'bts:widget-tip-face nil
  ;;                     )
  )


;; [ bts-github ] -- bts for GitHub

(use-package bts-github
  :ensure t
  :defer t
  :config
  (setq bts-github:ignore-labels '("duplicate" "invalid" "wontfix")
        ;; bts-github:max-lisp-eval-depth 6000
        ;; bts-github:max-specpdl-size 13000
        bts-github:summary-id-width 4
        bts-github:summary-label-decorating t
        bts-github:summary-label-width 15
        )

  ;; Faces
  (set-face-attribute 'bts-github:issue-comment-header-face nil
                      )
  (set-face-attribute 'bts-github:summary-label-decorating nil
                      )
  (set-face-attribute 'bts-github:summary-label-face nil
                      )
  )


(provide 'init-my-prog-bug-track-system)

;;; init-my-prog-bug-track-system.el ends here
