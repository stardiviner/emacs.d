;;; init-prog-bug-track-system.el --- init for Bug Tracking System
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'my-bts-prefix)
  (define-prefix-command 'my-bts-prefix))
(global-set-key (kbd "C-c b") 'my-bts-prefix)


;;; [ bts ] -- bug track system

(use-package pophint
  :ensure t
  :defer t)

;; (use-package bts
;;   :ensure t
;;   :defer t
;;   :bind (:map my-bts-prefix
;;               ("b" . bts:summary-open)
;;               ("t" . bts:ticket-new)
;;               ("P" . bts:project-new)
;;               ("p" . bts:project-update)
;;               ("r" . bts:project-remove)
;;               ("R" . bts:project-remove-all)
;;               ("q" . bts:query-new)
;;               ("Q" . bts:query-update)
;;               ("d" . bts:query-remove)
;;               ("D" . bts:query-remove-all))
;;   :cusotm ((bts:preferred-selection-method 'default)
;;            (bts:project-cache-file (expand-file-name ".bts/project" user-emacs-directory))
;;            (bts:query-cache-file (expand-file-name ".bts/query" user-emacs-directory))
;;            (bts:ticket-fetch-check-interval 3)
;;            (bts:ticket-multi-view-preferred t)
;;            ;; widget label
;;            (bts:widget-label-format " %s ")
;;            (bts:widget-label-prefix " ")
;;            (bts:widget-label-suffix " ")
;;            (bts:widget-require-mark "*")))

;; [ bts-github ] -- bts for GitHub

;; (use-package bts-github
;;   :ensure t
;;   :after bts
;;   :defer t
;;   :custom ((bts-github:ignore-labels '("duplicate" "invalid" "wontfix"))
;;            ;; bts-github:max-lisp-eval-depth 6000
;;            ;; bts-github:max-specpdl-size 13000
;;            (bts-github:summary-id-width 4)
;;            (bts-github:summary-label-decorating t)
;;            (bts-github:summary-label-width 15)))


(provide 'init-prog-bug-track-system)

;;; init-prog-bug-track-system.el ends here
