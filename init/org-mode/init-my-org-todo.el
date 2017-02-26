;;; init-my-org-todo.el --- init for Org Todo items
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


;;; [ Status ]

;;; statistics -> [1/10] or [15%]
(setq org-provide-todo-statistics t
      org-hierarchical-todo-statistics nil ; nil: cover recursive all entries.
      org-checkbox-hierarchical-statistics nil ; nil: covers recursive all entries.
      org-enforce-todo-checkbox-dependencies t
      )

;; Org-mode built-in TODOs dependencies, enforce parent and sub-tasks DONE.
(setq org-enforce-todo-dependencies t
      org-track-ordered-property-with-tag t
      org-agenda-dim-blocked-tasks t
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
        ;; Status: Getting Things Done
        (sequence "URGENT(u!)" "INPROGRESS(g!)" "TODO(t@/!)" "LATER(l!)" "NEXT(n!)" "SOMEDAY(s@/!)" "FAILED(x@/!)" "CANCELLED(C@/!)" "|" "DONE(d@/!)")
        ;; Clock
        (sequence "STARTED(!)" "|" "DONE(d@/!)")
        ;; Habit
        (sequence "HABIT(h)" "|" "DONE(d)")
        ;; Types
        (type "CODE(c@/!)" "PROJECT(P@/!)" "Org(o@/!)" "|" "DONE(d@/!)")
        ;; Code
        (sequence "BUG(b!)" "ISSUE(i!)" "ERROR(e!)" "FEATURE(f!)" "Pull-Request(p!)" "|" "DONE(d@/!)")
        ;; Work
        (type "WORK(w@/!)" "MEETING(m@/!)" "|" "DONE(d@/!)")
        ;; Learn
        (type "LEARN(N!)" "REVIEW(R!)" "|" "DONE(d@/!)")
        ;; Life
        (type "SEX(X@/!)" "|" "DONE(d@/!)")
        ;; org-trello
        ;; (type "TODO" "INPROGRESS" "|" "DONE")
        ))

(setq org-todo-keyword-faces
      '(;;; todo keywords
        ("TODO" :foreground "orange"
         :box '(:color "black" :line-width -1))
        ("URGENT" :foreground "red" :background "black"
         :box '(:color "black" :line-width -1))
        ("STARTED" :foreground "green"
         :box '(:color "red" :line-width -1))
        ("HABIT" :foreground "cyan" :background "black"
         :box '(:color "green" :line-width -1))
        ("SOMEDAY" :foreground "dim gray"
         :box '(:color "black" :line-width -1))
        ("INPROGRESS" :foreground "cyan"
         :box '(:color "black" :line-width -1))
        ("LATER" :foreground "dim gray" :background "black"
         :box '(:color "dark red" :line-width -1))
        ("DONE" :foreground "black"
         :strike-through t
         :box '(:color "black" :line-width -1))
        ("FAILED" :foreground "#444444"
         :underline "dark red"
         :box '(:color "black" :line-width -1))
        ("CANCELLED"
         :foreground "black"
         :strike-through t
         :box '(:color "black" :line-width -1))
        ;; code programming
        ("BUG" :foreground "red"
         :box '(:color "red" :line-width -1 :style nil))
        ("ISSUE" :foreground "red"
         :box '(:color "dark red" :line-width -1 :style nil))
        ("ERROR" :foreground "red"
         :box '(:color "red" :line-width -1 :style nil))
        ("FIXME" :foreground "black" :background "red"
         :box '(:color "dark red" :line-width -1 :style nil))
        ("FEATURE" :foreground "cyan"
         :box '(:color "cyan" :line-width -1 :style nil))
        ("Pull-Request" :foreground "yellow"
         :box '(:color "yellow" :line-width -1 :style nil))
        ;; types
        ("Org" :foreground "cyan" :background "#004A5D"
         :box '(:color "cyan" :line-width -1 :style nil))
        ("CODE" :foreground "white" :background "#004A5D"
         :box '(:color "cyan" :line-width -1 :style nil))
        ("PROJECT" :foreground "white" :background "#004A5D"
         :box '(:color "cyan" :line-width -1 :style nil))
        ;; life
        ("SEX" :foreground "deep pink"
         :box '(:color "deep pink" :line-width -1 :style nil))
        ;; work
        ("WORK" :foreground "orange"
         :box '(:color "black" :line-width -1 :style nil))
        ("MEETING" :foreground "cornflower blue"
         :box '(:color "cyan" :line-width -1 :style nil))
        ;; learn
        ("LEARN" :foreground "green yellow"
         :box '(:color "black" :line-width -1))
        ("REVIEW" :foreground "yellow"
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
