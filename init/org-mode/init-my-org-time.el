;;; init-my-org-time.el --- init for Org Time & Date
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Time Stamps ]


;;; [ Time Interval ]

(defun org-time-interval (&optional arg)
  "Set schedule and deadline time interval for headline.

Accepts universal argument \\<C-c C-x r> & \\[org-time-interval]."
  (interactive "P")
  ;; C-u is '(4) and C-u C-u is '(16)
  ;; (equal arg '(4))
  ;; So I need to use `(interactive "p")' for `(org-deadline)'.
  (org-schedule arg)
  ;; (org-deadline arg "+3d") ; this is not interactive for deadline.
  (org-deadline arg))

(define-key org-mode-map (kbd "C-c C-x r") 'org-time-interval)


;;; [ Effort Estimates ] -- [C-c C-x C-c] + [C-c C-x C-e]

;; to add an effort estimate "on the fly".
(add-hook 'org-clock-in-prepare-hook 'org-clock-modify-effort-estimate)
;; (remove-hook 'org-clock-in-hook 'org-clock-modify-effort-estimate)

;; setup column views for effort estimates
(setq org-columns-default-format "%25ITEM %TODO %3PRIORITY %TAGS %8Effort(Effort){:}"
      ;; org-global-properties
      org-agenda-columns-add-appointments-to-effort-sum t
      org-agenda-columns-compute-summary-properties t
      )


;;; [ org-time-budgets ]

(use-package org-time-budgets
  ;; :ensure t
  :config
  (setq org-time-budgets
        '((:title "My Learning Plan" :tag "+learn" :budget "35:00" :block 'week)
          ;; (:title "" :tags "+play" :budget "30:00" :block 'weekend)

          (:title "Emacs" :tag "+learn" :budget "21:00" :block 'week)
          (:title "Ruby on Rails" :tag "+learn" :budget "21:00" :block 'week)
          (:title "Ruby" :tag "+learn" :budget "21:00" :block 'week)
          (:title "Web" :tag "+learn" :budget "21:00" :block 'week)
          (:title "Julia" :tag "+learn" :budget "15:00" :block 'week)
          (:title "Lisp" :tag "+learn" :budget "10:00" :block 'week)
          (:title "Clojure" :tag "+learn" :budget "14:00" :block 'week)
          ))


  ;; adding `org-time-budgets' to your Agenda.
  (add-to-list 'org-agenda-custom-commands
               '(("a" "Agenda"
                  ((agenda ""
                           ((org-agenda-sorting-strategy
                             '(habit-down time-up priority-down category-keep user-defined-up))))
                   (org-time-budgets-for-agenda)))))
  )



(provide 'init-my-org-time)

;;; init-my-org-time.el ends here
