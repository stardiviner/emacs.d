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
