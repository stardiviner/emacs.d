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

(setq org-effort-property "Effort"
      org-time-clocksum-use-effort-durations nil
      )


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
        (sequence "Urgent(u!)" "INPROGRESS(g!)" "TODO(t@/!)" "LATER(l!)" "SOMEDAY(s!)" "FAILED(x@/!)" "CANCELLED(C@/!)" "|" "DONE(d@/!)")
        ;; Clock
        (sequence "STARTED(!)" "|" "DONE(d@/!)")
        ;; Habit
        (sequence "HABIT(h)" "|" "DONE(d)")
        ;; Types
        (type "code(c@/!)" "project(p@/!)" "Org(o@/!)" "|" "DONE(d@/!)")
        ;; Code
        (sequence "BUG(b!)" "ISSUE(i!)" "ERROR(e!)" "FEATURE(f!)" "|" "DONE(d@/!)")
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
        ("DONE" :foreground "black" :background nil
         :weight bold :strike-through t
         :box '(:color "black" :line-width -1))
        ("FAILED" :foreground "#444444" :background "orange"
         :weight bold :underline "dark red"
         :box '(:color "black" :line-width -1))
        ("CANCELLED" :foreground "#444444" :background "orange"
         :weight bold :strike-through t
         :box '(:color "black" :line-width -1))
        ;; FIXME:
        ("BUG" :foreground "red" :background nil
         :weight bold
         :box '(:color "red" :line-width -1 :style nil))
        ("ISSUE" :foreground "red" :background nil
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
              ;;; types
        ("Org" :foreground "cyan" :backgrund "#004A5D"
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
        ("Learn" :foreground "dark orange" :background nil
         :box '(:color "black" :line-width -1))
        ("Learning" :foreground "green yellow" :background nil
         :box '(:color "black" :line-width -1))
        ("Review" :foreground "yellow" :background nil
         :box '(:color "black" :line-width -1))
        ))


;;; bind key [C-l] to locate to current time: "now -----" in Org-Agenda buffer.
(defun my-org-agenda-jump-to-current-time ()
  "Jump to current time now."
  (interactive)
  (goto-char
   (text-property-any (point-min) (point-max)
                      'face 'org-agenda-current-time))
  (recenter-top-bottom)
  )

(define-key org-agenda-mode-map (kbd "C-l") 'my-org-agenda-jump-to-current-time)


;;; [ Capture - Refile - Archive ]

(require 'org-capture)

(setq org-default-notes-file
      (concat org-directory "/Capture/notes.org"))

(setq org-capture-templates
      '(("c" "Capture"
         entry (file+headline "~/Org/Capture/Capture.org" "Capture")
         "\n* TODO %^{prompt}\n\n%i\n\n%a\n\n%?"
         :prepend t
         :empty-lines 1
         )

        ;; Tasks
        ("t" "Add a task into Tasks"
         entry (file+headline "~/Org/Tasks.org" "Tasks")
         "\n* TODO %^{prompt} [/]\n\n%?\n\n"
         :empty-lines 1
         )
        ("i" "Clock in a New Task"
         entry (file+headline "~/Org/Tasks.org" "Tasks")
         "\n* TODO %^{prompt} [/]\n\n%?\n\n"
         :empty-lines 1
         :clock-in t :clock-resume t
         )

        ;; Bookmark
        ("m" "Add an URL to bookmarks database"
         entry (file+headline "~/Org/Wiki/Data/Bookmarks/Bookmarks.org" "Capture")
         "\n* %^{prompt}\n\n%A\n\n%?\n\n"
         :empty-lines 1
         )

        ;; TODO: Contacts
        ("c" "Contacts"
         entry (file+headline "~/Org/Contacts/Contacts.org")
         "* %(org-contacts-template-name) %^g
%(org-contacts-template-email)
:PROPERTIES:
:NAME:
:NICK-NAME:
:BIRTHDAY: %:date
:URL: %:url
:EMAIL: %?
:MOBILE:
:WORK:
:SKILLS:
:HOME:
:COMPANY:
:ADDRESS:
:NOTE:
:END:")
        
        ;; org-passwords
        ;; FIXME:
        ("p" "password"
         entry (file+headline "~/Git/dotfiles/passwords.gpg" "Accounts")
         "* %^{Title}\n  %^{URL}p %^{USERNAME}p %^{PASSWORD}p %^{EMAIL}p"
         :empty-lines 1
         )

        ;; Issues, Bugs, Features
        ("b" "Bug"
         entry (file+olp "~/Org/Projects/Programming.org" "Computer" "Bugs")
         "\n* BUG %^{prompt}\n\n%i\n\n%?\n\n"
         :empty-lines 1)
        ("i" "Issue"
         entry (file+olp "~/Org/Projects/Programming.org" "Computer" "Issues")
         "\n* ISSUE %^{prompt}\n\n%i\n\n%?\n\n"
         :empty-lines 1)
        ("f" "Feature"
         entry (file+olp "~/Org/Projects/Programming.org" "Computer" "Features")
         "\n* FEATURE %^{prompt}\n\n%i\n\n%?\n\n"
         :empty-lines 1)
        ))


;; To define special keys to capture to a particular template without
;; going through the interactive template selection, you can create your
;; key binding like this:
;;
;; (global-set-key (kbd "C-c x")
;;                 '(lambda () (interactive) (org-capture nil "x")))


(if (featurep 'helm)
    (define-key my-org-prefix (kbd "c") 'helm-org-capture-templates)
  (define-key my-org-prefix (kbd "c") 'org-capture)
  (define-key org-mode-map (kbd "C-c c") 'org-capture))


;;;_* Refile

(setq org-refile-keep nil
      org-refile-markers nil
      ;;; Refile targets include this file and any file contributing to the
      ;;; agenda - up to 5 levels deep
      org-refile-targets '((nil :maxlevel . 5)
                           (org-agenda-files :maxlevel . 5))
      ;;; Targets start with the file name - allows creating level 1 tasks
      ;; org-refile-use-outline-path (quote file)
      org-outline-path-complete-in-steps t
      ;; org-refile-use-outline-path ; TODO
      org-refile-target-verify-function t
      org-refile-use-outline-path nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-active-region-within-subtree t
      )

;;;_* Archive

(setq org-archive-location "%s_archive::"
      org-archive-save-context-info '(time file olpath category todo itags ltags)
      org-archive-mark-done nil
      org-archive-stamp-time t
      org-archive-reversed-order nil
      )



;;; [ Habit ]

;;; Example:
;; * TODO write journal
;;   SCHEDULED: <2014-01-28 Tue .+1d/3d>
;;   - State "DONE"       from "TODO"       [2014-01-27 Mon 20:50]
;;   - State "DONE"       from "TODO"       [2013-12-03 Tue 06:37]
;;   - State "DONE"       from "TODO"       [2013-12-02 Mon 21:54]
;;   :PROPERTIES:
;;   :LAST_REPEAT: [2014-01-27 Mon 20:50]
;;   :STYLE:    habit
;;   :END:

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

(use-package org-habit
  :config
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
  )

;; create an key binding for all necessary steps for create a habit.
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
  ;; (org-set-property "LOGGING" "TODO DONE(!)")
  )

(define-key org-mode-map (kbd "C-c C-x h") 'org-habit-apply)

;;; Stuck Project

(setq org-stuck-projects
      '("+PROJECT/-MAYBE-DONE" ("NEXT" "TODO") ("@SHOP")
        "\\<IGNORE\\>"))



(provide 'init-my-org-todo)

;;; init-my-org-todo.el ends here
