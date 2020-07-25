;;; init-org-todo.el --- init for Org Todo items
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; statistics -> [1/10] or [15%]
(setq org-checkbox-hierarchical-statistics nil)

;;; todo task dependencies

;; Org-mode built-in TODOs dependencies, enforce parent and sub-tasks DONE.
(setq org-enforce-todo-dependencies t)

(setq org-track-ordered-property-with-tag t)
(add-to-list 'org-default-properties "ORDERED")

;;; [ Log ]

(setq org-log-into-drawer t
      org-log-note-clock-out t
      org-log-done 'time
      org-log-done-with-time t
      org-log-repeat 'note
      org-log-refile 'time)

;;; [ Effort ]

;;; TODOs status
;;
;; `|` separate finished and unfinished two statuses, will add timestamp when finished.
;; `(t)` set shortcut
;; `(d!)` add timestamp
;; `(d@)` need add note declaration
;; `(d@/!)` add timestamp and note
(setq org-todo-keywords
      '(;; Status: Getting Things Done
        (sequence "TODO(t@/!)" "NEXT(n!)"
                  "INPROGRESS(g@/!)" "DELAYED(z@/!)" "LATER(l!)" "SOMEDAY(S@/!)"
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
        (type "WORK(w@/!)" "MEETING(m@/!)" "JOB(j@/!)" "BUSINESS(@/!)" "|" "DONE(d@/!)")
        ;; Learn
        (type "LEARN(N!)" "REVIEW(R!)" "|" "DONE(d@/!)")
        ;; Idea
        (type "IDEA(I@/!)" "|" "DONE(d@/!)")
        ;; Life
        (type "SEX(X@/!)" "|" "DONE(d@/!)")
        ;; `org-trello'
        ;; (type "TODO" "INPROGRESS" "|" "DONE")
        ;; Taobao
        (type "BUY(B@/!)" "SELL(L@/!)" "|" "DONE(d@/!)")
        ))

(defface org-todo-keyword-todo-face
  '((t :foreground "orange" :weight bold))
  "Face for Org TODO keyword."
  :group 'org-faces)
(defface org-todo-keyword-done-face
  '((t :foreground "black" :weight bold))
  "Face for Org TODO keyword."
  :group 'org-faces)

(setq org-todo-keyword-faces
      '(;;; todo keywords
        ("TODO" . org-todo-keyword-todo-face)
        ("REPEAT" . org-todo-keyword-todo-face)
        ("HABIT" . org-todo-keyword-todo-face)
        ("NEXT" . org-todo-keyword-todo-face)
        ("STARTED" . org-todo-keyword-todo-face)
        ("INPROGRESS" . org-todo-keyword-todo-face)
        ("DELAYED" . org-todo-keyword-todo-face)
        ("LATER" . org-todo-keyword-todo-face)
        ("SOMEDAY" . org-todo-keyword-todo-face)
        ("DONE" . org-todo-keyword-done-face)
        ("FAILED" . org-todo-keyword-done-face)
        ("CANCELLED" . org-todo-keyword-done-face)
        ;; code programming
        ("CODE" . org-todo-keyword-todo-face)
        ("BUG" . org-todo-keyword-todo-face)
        ("ISSUE" . org-todo-keyword-todo-face)
        ("ERROR" . org-todo-keyword-todo-face)
        ("FEATURE" . org-todo-keyword-todo-face)
        ("Pull-Request" . org-todo-keyword-todo-face)
        ("REVIEW" . org-todo-keyword-todo-face)
        ("SECURITY" . org-todo-keyword-todo-face)
        ;; life
        ("SEX" . org-todo-keyword-todo-face)
        ;; work
        ("WORK" . org-todo-keyword-todo-face)
        ("JOB" . org-todo-keyword-todo-face)
        ("BUSINESS" . org-todo-keyword-todo-face)
        ("MEETING" . org-todo-keyword-todo-face)
        ;; learn
        ("LEARN" . org-todo-keyword-todo-face)
        ;; idea & project
        ("IDEA" . org-todo-keyword-todo-face)
        ("PROJECT" . org-todo-keyword-todo-face)))


;;; [ Habit ]

(use-package org-habit
  :after org
  :custom ((org-habit-graph-column 70)
           (org-habit-today-glyph ?>)
           (org-habit-completed-glyph ?✔))
  :config
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
    ;; (org-set-property "LOGGING" "TODO DONE(!)")
    )

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
             '("PROJECT" . org-todo-keyword-todo-face))


;;; [ Task Dependencies ]

(add-to-list 'org-default-properties "ID")
(add-to-list 'org-default-properties "TRIGGER")
(add-to-list 'org-default-properties "BLOCKER")

;;; [ org-edna ] -- Extensible Dependencies 'N' Actions.

(use-package org-edna
  :ensure t
  :defer t
  :delight org-edna-mode
  :after org
  :hook (org-mode . org-edna-mode))



(provide 'init-org-todo)

;;; init-org-todo.el ends here
