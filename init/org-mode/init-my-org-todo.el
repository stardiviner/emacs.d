;;; init-my-org-todo.el --- init for Org Todo items
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


;;; [ Status ]

;;; statistics -> [1/10] or [15%]
(setq org-provide-todo-statistics t
      org-hierarchical-todo-statistics nil ; nil: cover recursive all entries.
      org-checkbox-hierarchical-statistics nil ; nil: covers recursive all entries.
      org-enforce-todo-dependencies nil ; enforce parent and sub-tasks DONE
      )

;;; time repeat
(setq org-todo-repeat-to-state "REPEAT"
      org-log-repeat 'time
      org-agenda-repeating-timestamp-show-all t
      )


;;; [ Log ]

(setq org-log-done 'time
      org-log-into-drawer t
      )

;;; [ Effort ]

(setq org-time-clocksum-use-effort-durations nil)


;;; TODOs status
;;
;; `|` separate finished and unfinished two statuses, will add timestamp when finished.
;; `(t)` set shortcut
;; `(d!)` add timestamp
;; `(d@)` need add note declaration
;; `(d@/!)` add timestamp and note
(setq org-todo-keywords
      '(
        ;; Status
        (sequence "Urgent(u!)" "INPROGRESS(g!)" "TODO(t@/!)" "LATER(l!)" "SOMEDAY(s@/!)" "FAILED(x@/!)" "CANCELLED(C@/!)" "|" "DONE(d@/!)")
        ;; Clock
        (sequence "STARTED(!)" "|" "DONE(d@/!)")
        ;; Habit
        (sequence "HABIT(h)" "|" "DONE(d)")
        ;; Types
        (type "code(c@/!)" "project(P@/!)" "Org(o@/!)" "|" "DONE(d@/!)")
        ;; Code
        (sequence "BUG(b!)" "ISSUE(i!)" "ERROR(e!)" "FEATURE(f!)" "Pull-Request(p!)" "|" "DONE(d@/!)")
        ;; Work
        (type "Work(w@/!)" "Meeting(m@/!)" "|" "DONE(d@/!)")
        ;; Learn
        (type "Learn(n!)" "Review(r!)" "|" "DONE(d@/!)")
        ;; Life
        (type "SEX(X@/!)" "|" "DONE(d@/!)")
        ;; org-trello
        ;; (type "TODO" "INPROGRESS" "|" "DONE")
        ))

(setq org-todo-keyword-faces
      '(;;; todos
        ("TODO" :foreground "orange"
         :weight bold
         :box '(:color "black" :line-width -1))
        ("Urgent" :foreground "red" :background "black"
         :weight bold
         ;; :overline "red"
         :box '(:color "black" :line-width -1 :style nil)
         )
        ("STARTED" :foreground "green"
         :weight bold
         :box '(:color "red" :line-width -1))
        ("HABIT" :foreground "cyan" :background "black"
         :weight bold
         :box '(:color "green" :line-width -1))
        ("SOMEDAY" :foreground "dim gray"
         :weight bold
         :box '(:color "black" :line-width -1))
        ("INPROGRESS" :foreground "cyan"
         :weight bold
         :box '(:color "black" :line-width -1))
        ("LATER" :foreground "dim gray" :background "black"
         :weight bold
         :box '(:color "dark red" :line-width -1))
        ("DONE" :foreground "black"
         :weight bold :strike-through t
         :box '(:color "black" :line-width -1))
        ("FAILED" :foreground "#444444" :background "orange"
         :weight bold :underline "dark red"
         :box '(:color "black" :line-width -1))
        ("CANCELLED" :foreground "#444444" :background "orange"
         :weight bold :strike-through t
         :box '(:color "black" :line-width -1))
        ;; FIXME:
        ("BUG" :foreground "red"
         :weight bold
         :box '(:color "red" :line-width -1 :style nil))
        ("ISSUE" :foreground "red"
         :weight bold
         :box '(:color "dark red" :line-width -1 :style nil))
        ("ERROR" :foreground "red"
         :weight bold
         :box '(:color "red" :line-width -1 :style nil))
        ("FIXME" :foreground "black" :background "red"
         :weight bold
         :box '(:color "dark red" :line-width -1 :style nil))
        ("FEATURE" :foreground "cyan"
         :weight bold
         :box '(:color "cyan" :line-width -1 :style nil))
        ("Pull-Request" :foreground "yellow"
         :weight bold
         :box '(:color "yellow" :line-width -1 :style nil))
              ;;; types
        ("Org" :foreground "cyan" :background "#004A5D"
         :weight bold
         :box '(:color "cyan" :line-width -1 :style nil))
        ("code" :foreground "white" :background "#004A5D"
         :box '(:color "cyan" :line-width -1 :style nil))
        ("project" :foreground "white" :background "#004A5D"
         :box '(:color "cyan" :line-width -1 :style nil))
        ;; life
        ("SEX" :foreground "deep pink"
         :weight bold
         :box '(:color "deep pink" :line-width -1 :style nil))
        ("Outside" :foreground "yellow"
         :box '(:color "yellow" :line-width -1 :style nil))
        ;; work
        ("Work" :foreground "orange"
         :box '(:color "black" :line-width -1 :style nil))
        ("Meeting" :foreground "cornflower blue"
         :box '(:color "cyan" :line-width -1 :style nil))
        ;; learn
        ("Learn" :foreground "dark orange"
         :box '(:color "black" :line-width -1))
        ("Learning" :foreground "green yellow"
         :box '(:color "black" :line-width -1))
        ("Review" :foreground "yellow"
         :box '(:color "black" :line-width -1))
        ))


;;; auto remove priority after mark task done.

;; (defun my/org-remove-preiority ()
;;   (if (= (org-get-priority (match-string 0)) 0)
;;       (org-priority 'remove)))
;;
;; (add-hook 'org-after-todo-statistics-hook 'my/org-remove-preiority)


;;; [ Habit ]

(require 'org-habit)

(setq org-habit-show-habits t      ; show habits in agenda.
      org-habit-show-all-today nil   ; show all habits' consistency graph in today
      org-habit-show-habits-only-for-today t
      org-habit-graph-column 70
      org-habit-preceding-days 14
      org-habit-following-days 7
      org-habit-today-glyph ?⇨
      org-habit-completed-glyph ?✔
      )

(set-face-attribute 'org-habit-clear-face nil ; for days that task shouldn't be done yet
                    :background "dark green"
                    :foreground "gray")
(set-face-attribute 'org-habit-clear-future-face nil ; for future days that task shouldn't be done yet
                    :background "dark gray")
(set-face-attribute 'org-habit-ready-face nil ; for days that task is done
                    :background "#333333"
                    :foreground "gray")
(set-face-attribute 'org-habit-ready-future-face nil ; task should be start
                    :background "dodger blue")
(set-face-attribute 'org-habit-alert-future-face nil ; task is going to be due
                    :background "dark orange")
(set-face-attribute 'org-habit-alert-face nil ; task is due
                    :background "dark red")
(set-face-attribute 'org-habit-overdue-face nil ; for days that task is overdue before today
                    :background "saddle brown"
                    :foreground "white")
(set-face-attribute 'org-habit-overdue-future-face nil ; task in future
                    :background "slate gray")

;; set task to habit
(defun org-habit-apply ()
  "Apply org-habit on this task."
  (interactive)
  (beginning-of-line)
  (org-todo "HABIT")
  ;; The format-time-string code is correct.
  ;; (format-time-string "%Y-%m-%d %H:%M .+1d" (current-time))
  ;; (org-schedule nil (format-time-string "%Y-%m-%d %H:%M" (current-time))) ; deactive
  (org-schedule nil) ; interactive
  (save-excursion
    (next-line) (beginning-of-line)
    (when (looking-at "SCHEDULED: [^>]*\\(>\\)")
      (goto-char (match-beginning 1))
      (insert (concat
               " .+"
               (read-string "Minimum interval: ")
               "d"
               "/"
               (read-string "Maximum interval: ")
               "d"))))

  (org-set-property "STYLE" "habit")
  (org-set-property "LOGGING" "TODO DONE(!)")
  )

(define-key org-mode-map (kbd "C-c C-x h") 'org-habit-apply)

;;; insert habit
(defun org-insert-habit ()
  "Insert a new TODO subheading and set its properties so that it becomes a habit."
  (interactive)
  (beginning-of-line)
  (org-insert-todo-subheading nil)
  (org-schedule nil (format-time-string "%Y-%m-%d" (current-time)))
  (save-excursion
    (search-forward ">")
    (backward-char)
    (insert (concat
             " .+"
             (read-string "Minimum interval: ")
             "/"
             (read-string "Maximum interval: "))))
  (org-set-property "STYLE" "habit")
  (org-set-property "LOGGING" "TODO DONE(!)"))

(define-key org-mode-map (kbd "C-c C-x H") 'org-insert-habit)

;;; Stuck Project

(setq org-stuck-projects
      '("+PROJECT/-MAYBE-DONE" ("NEXT" "TODO") ("@SHOP")
        "\\<IGNORE\\>"))


;;; [ inline task ]

(require 'org-inlinetask)

(setq org-inlinetask-default-state "TODO"
      org-inlinetask-show-first-star nil
      ;; org-inlinetask-min-level 15
      )


(provide 'init-my-org-todo)

;;; init-my-org-todo.el ends here
