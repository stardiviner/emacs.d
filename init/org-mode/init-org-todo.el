;;; init-org-todo.el --- init for Org Todo items
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Status ]

;;; statistics -> [1/10] or [15%]

;; Org-mode built-in TODOs dependencies, enforce parent and sub-tasks DONE.
(setq org-enforce-todo-dependencies t)

(setq org-track-ordered-property-with-tag t)
(add-to-list 'org-default-properties "ORDERED")

;;; time repeat
(setq org-todo-repeat-to-state nil ; or "REPEAT". Leave the todo state as it is, like "HABIT".
      org-agenda-prefer-last-repeat nil
      org-log-repeat 'time)


;;; [ Log ]

(setq org-log-into-drawer t
      org-log-note-clock-out t
      org-log-done 'time
      org-log-done-with-time t
      org-log-redeadline 'time
      org-log-reschedule 'time
      org-log-refile 'time
      org-log-repeat 'time)

;;; [ Effort ]

;;; TODOs status
;;
;; `|` separate finished and unfinished two statuses, will add timestamp when finished.
;; `(t)` set shortcut
;; `(d!)` add timestamp
;; `(d@)` need add note declaration
;; `(d@/!)` add timestamp and note
(setq org-todo-keywords
      '(
        ;; Status: Getting Things Done
        (sequence "TODO(t@/!)" "NEXT(n!)"
                  "URGENT(u@/!)" "INPROGRESS(g@/!)" "LATER(l!)" "SOMEDAY(S@/!)"
                  "|" "DONE(d@/!)" "FAILED(x@/!)" "CANCELLED(k@/!)")
        ;; Clock
        (sequence "STARTED(!)" "|" "DONE(d@/!)")
        ;; Repeat tasks with active timestamps
        (sequence "REPEAT(r!)" "|" "DONE(d@/!)")
        ;; Habit
        (sequence "HABIT(h)" "|" "DONE(d)")
        ;; Code
        (sequence "CODE(c@/!)" "BUG(b@/!)" "ISSUE(i@/!)" "ERROR(e@/!)" "FEATURE(f@/!)" "Pull-Request(p@/!)" "SECURITY(s@/!)" "|" "DONE(d@/!)")
        ;; Work
        (type "WORK(w@/!)" "MEETING(m@/!)" "JOB(j@/!)" "|" "DONE(d@/!)")
        ;; Learn
        (type "LEARN(N!)" "REVIEW(R!)" "|" "DONE(d@/!)")
        ;; Life
        (type "SEX(X@/!)" "|" "DONE(d@/!)")
        ;; org-trello
        ;; (type "TODO" "INPROGRESS" "|" "DONE")
        ))

(defface org-todo-keyword-todo-face
  '((t :foreground "orange" :family "Comic Sans MS"
       :weight bold :box '(:color "dim gray" :line-width -1)))
  "Face for Org TODO keyword."
  :group 'org-faces)
(defface org-todo-keyword-repeat-face
  '((t :foreground "cyan" :family "Comic Sans MS"
       :weight bold :box '(:color "dim gray" :line-width -1)))
  "Face for Org REPEAT keyword."
  :group 'org-faces)
(defface org-todo-keyword-habit-face
  '((t :foreground "cyan" :family "Comic Sans MS"
       :weight bold :box '(:color "dim gray" :line-width -1)))
  "Face for Org HABIT keyword."
  :group 'org-faces)
(defface org-todo-keyword-next-face
  '((t :foreground "yellow" :family "Comic Sans MS"
       :weight bold :box '(:color "dim gray" :line-width -1)))
  "Face for Org NEXT keyword."
  :group 'org-faces)
(defface org-todo-keyword-inprogress-face
  '((t :foreground "cyan" :family "Comic Sans MS"
       :weight bold :box '(:color "dim gray" :line-width -1)))
  "Face for Org INPROGRESS keyword."
  :group 'org-faces)
(defface org-todo-keyword-someday-face
  '((t :foreground "gray" :family "Comic Sans MS"
       :weight bold :box '(:color "dim gray" :line-width -1)))
  "Face for Org SOMEDAY keyword."
  :group 'org-faces)
(defface org-todo-keyword-later-face
  '((t :foreground "dim gray" :family "Comic Sans MS"
       :weight bold :box '(:color "dim gray" :line-width -1)))
  "Face for Org LATER keyword."
  :group 'org-faces)
(defface org-todo-keyword-done-face
  '((t :foreground "black" :family "Comic Sans MS"
       :strike-through t :weight bold :box '(:color "dim gray" :line-width -1)))
  "Face for Org DONE keyword."
  :group 'org-faces)
(defface org-todo-keyword-failed-face
  '((t :foreground "#444444" :family "Comic Sans MS"
       :strike-through "dark red" :weight bold :box '(:color "dim gray" :line-width -1)))
  "Face for Org FAILED keyword."
  :group 'org-faces)
(defface org-todo-keyword-cancelled-face
  '((t :foreground "black" :family "Comic Sans MS"
       :strike-through t :weight bold :box '(:color "dim gray" :line-width -1)))
  "Face for Org CANCELLED keyword."
  :group 'org-faces)

(defface org-todo-keyword-code-face
  '((t :foreground "DodgerBlue" :family "Comic Sans MS"
       :weight bold :box '(:color "dim gray" :line-width -1)))
  "Face for Org CODE keyword."
  :group 'org-faces)
(defface org-todo-keyword-bug-face
  '((t :foreground "dark red" :family "Comic Sans MS"
       :weight bold :box '(:color "dim gray" :line-width -1)))
  "Face for Org BUG keyword."
  :group 'org-faces)
(defface org-todo-keyword-issue-face
  '((t :foreground "blue" :family "Comic Sans MS"
       :weight bold :box '(:color "dim gray" :line-width -1)))
  "Face for Org ISSUE keyword."
  :group 'org-faces)
(defface org-todo-keyword-error-face
  '((t :foreground "red" :family "Comic Sans MS"
       :weight bold :box '(:color "dim gray" :line-width -1)))
  "Face for Org ERROR keyword."
  :group 'org-faces)
(defface org-todo-keyword-feature-face
  '((t :foreground "cyan" :family "Comic Sans MS"
       :weight bold :box '(:color "dim gray" :line-width -1)))
  "Face for Org FEATURE keyword."
  :group 'org-faces)
(defface org-todo-keyword-pull-request-face
  '((t :foreground "yellow" :family "Comic Sans MS"
       :weight bold :box '(:color "dim gray" :line-width -1)))
  "Face for Org Pull-Request keyword."
  :group 'org-faces)
(defface org-todo-keyword-review-face
  '((t :foreground "yellow" :family "Comic Sans MS"
       :weight bold :box '(:color "dim gray" :line-width -1)))
  "Face for Org REVIEW keyword."
  :group 'org-faces)
(defface org-todo-keyword-security-face
  '((t :foreground "red" :family "Comic Sans MS"
       :weight bold :box '(:color "dim gray" :line-width -1)))
  "Face for Org REVIEW keyword."
  :group 'org-faces)

(defface org-todo-keyword-sex-face
  '((t :foreground "deep pink" :family "Comic Sans MS"
       :weight bold :box '(:color "dim gray" :line-width -1)))
  "Face for Org SEX keyword."
  :group 'org-faces)
(defface org-todo-keyword-work-face
  '((t :foreground "cornflower blue" :family "Comic Sans MS"
       :weight bold :box '(:color "dim gray" :line-width -1)))
  "Face for Org WORK keyword."
  :group 'org-faces)
(defface org-todo-keyword-learn-face
  '((t :foreground "green yellow" :family "Comic Sans MS"
       :weight bold :box '(:color "dim gray" :line-width -1)))
  "Face for Org LEARN keyword."
  :group 'org-faces)

(setq org-todo-keyword-faces
      '(;;; todo keywords
        ("TODO" . org-todo-keyword-todo-face)
        ("REPEAT" . org-todo-keyword-repeat-face)
        ("HABIT" . org-todo-keyword-habit-face)
        ("NEXT" . org-todo-keyword-next-face)
        ("SOMEDAY" . org-todo-keyword-someday-face)
        ("INPROGRESS" . org-todo-keyword-inprogress-face)
        ("LATER" . org-todo-keyword-later-face)
        ("DONE" . org-todo-keyword-done-face)
        ("FAILED" . org-todo-keyword-failed-face)
        ("CANCELLED" . org-todo-keyword-cancelled-face)
        ;; code programming
        ("CODE" . org-todo-keyword-code-face)
        ("BUG" . org-todo-keyword-bug-face)
        ("ISSUE" . org-todo-keyword-issue-face)
        ("ERROR" . org-todo-keyword-error-face)
        ("FEATURE" . org-todo-keyword-feature-face)
        ("Pull-Request" . org-todo-keyword-pull-request-face)
        ("REVIEW" . org-todo-keyword-review-face)
        ("SECURITY" . org-todo-keyword-security-face)
        ;; life
        ("SEX" . org-todo-keyword-sex-face)
        ;; work
        ("WORK" . org-todo-keyword-work-face)
        ("JOB" . org-todo-keyword-work-face)
        ;; learn
        ("LEARN" . org-todo-keyword-learn-face)
        ))


;;; [ Habit ]

(use-package org-habit
  :defer t
  :init (setq org-habit-show-habits t      ; show habits in agenda.
              org-habit-show-all-today nil   ; show all habits' consistency graph in today
              org-habit-show-habits-only-for-today t
              org-habit-graph-column 70
              org-habit-preceding-days 14
              org-habit-following-days 7
              org-habit-today-glyph ?>
              org-habit-completed-glyph ?✔)

  ;; set task to habit
  (defun org-habit-apply ()
    "Apply org-habit on this task."
    (interactive)
    (beginning-of-line)
    (org-todo "HABIT")
    ;; (format-time-string "%Y-%m-%d %a %H:%M .+1d" (current-time))
    (org-schedule nil (format-time-string "%Y-%m-%d %a %H:%M" (current-time)))
    (save-excursion
      (next-line) (beginning-of-line)
      (when (looking-at "\\( \\)*SCHEDULED: [^>]*\\(>\\)")
        (goto-char (match-beginning 2))
        (insert (concat
                 " .+"
                 (read-string "Minimum interval (d,w,m,y): ") "/"
                 (read-string "Maximum interval (d,w,m,y): ")))))
    (if (yes-or-no-p "Set schedule delay day? ")
        (org-schedule '(16)))
    (org-set-property "STYLE" "habit")
    (org-set-property "LOGGING" "TODO DONE(!)"))

  (define-key org-mode-map (kbd "C-c C-x M-h") 'org-habit-apply)
  ;; :config
  ;; ;; "Face for future days on which a task shouldn't be done yet."
  ;; (set-face-attribute 'org-habit-clear-future-face nil
  ;;                     :background "#222222")
  ;; ;; "Face for days on which a task shouldn't be done yet."
  ;; (set-face-attribute 'org-habit-clear-face nil
  ;;                     :background "black")
  ;; ;; "Face for days on which a task should start to be done."
  ;; (set-face-attribute 'org-habit-ready-future-face nil
  ;;                     :background "#444444")
  ;; ;; "Face for days on which a task should start to be done."
  ;; (set-face-attribute 'org-habit-ready-face nil
  ;;                     :background "#888888")
  ;; ;; "Face for days on which a task is due."
  ;; (set-face-attribute 'org-habit-alert-face nil
  ;;                     :background "orange")
  ;; ;; "Face for days on which a task is due."
  ;; (set-face-attribute 'org-habit-alert-future-face nil
  ;;                     :background "DarkOrange2")
  ;; ;; "Face for days on which a task is overdue."
  ;; (set-face-attribute 'org-habit-overdue-face nil
  ;;                     :background "dark red")
  ;; ;; "Face for days on which a task is overdue."
  ;; (set-face-attribute 'org-habit-overdue-future-face nil
  ;;                     :background "DarkSlateBlue")
  )

;;; Stuck Project: Find projects you need to review. [C-c o a #]

(setq org-stuck-projects
      '("+PROJECT/-DONE" ("NEXT" "NEXTACTION" "TODO") nil ""))

(add-to-list 'org-todo-keywords
             '(type "PROJECT(P@/!)" "|" "DONE(d@/!)"))
(add-to-list 'org-todo-keyword-faces
             '("PROJECT" . org-todo-keyword-work-face))


;;; [ inline task ]

(use-package org-inlinetask
  :defer t
  :init (setq org-inlinetask-default-state "TODO"
              org-inlinetask-show-first-star nil))

;;; [ Task Dependencies ]

(add-to-list 'org-default-properties "ID")
(add-to-list 'org-default-properties "TRIGGER")
(add-to-list 'org-default-properties "BLOCKER")

;;; [ org-edna ] -- Extensible Dependencies 'N' Actions.

(use-package org-edna
  :ensure t
  :defer t
  :init (org-edna-load))



(provide 'init-org-todo)

;;; init-org-todo.el ends here
