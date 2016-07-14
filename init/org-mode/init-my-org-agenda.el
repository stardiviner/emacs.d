;;; init-my-org-agenda.el --- init for Org Agenda
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(require 'org-agenda)

;; Agenda Views
(setq org-agenda-prefix-format
      '((agenda . " ┠ %i %-12:c%?-12t% s")
        (timeline . "  % s")
        (todo . " %i %-12:c")
        (search . " %i %-12:c")
        (tags . " %i %-12:c")))

(setq org-agenda-block-separator ?=
      org-agenda-compact-blocks t
      )

;;; sorting strategy
(setq org-agenda-sorting-strategy
      '((agenda time-up priority-down deadline-up ts-up habit-down category-keep)
        (todo priority-down category-keep)
        (tags priority-down category-keep)
        (search category-keep))
      org-agenda-sorting-strategy-selected
      '(time-up priority-down deadline-up ts-up habit-down category-keep)
      )

;;; Time Grid
(setq org-agenda-use-time-grid t)
(setq org-agenda-timegrid-use-ampm t)
(setq org-agenda-show-current-time-in-grid t)


;; include `diary-file' from `calendar'
(setq org-agenda-include-diary nil
      diary-file "~/Org/Diary/Diary.org"
      org-agenda-diary-file 'diary-file
      ;; org-agenda-insert-diary-strategy 'date-tree
      )

;;; Icon
;; How to make icon with GIMP?
;;
;; 1. "scale" image to [16x16] pixels.
;; 2. "select by color" -> "color to alpha"
;; 3. export to .xpm format. (or .png)

(setq org-agenda-category-icon-alist
      '(("Diary" "~/.emacs.d/resources/icon/diary.xpm" nil nil :ascent center)
        ;; ("Todo" "~/.emacs.d/resources/icon/todo.xpm" nil nil :ascent center)
        ;; ("Todo" "~/.emacs.d/resources/icon/Todo.xpm" nil nil :ascent center)
        ("Todo" "~/.emacs.d/resources/icon/list.xpm" nil nil :ascent center)
        ("Star" "~/.emacs.d/resources/icon/star.xpm" nil nil :ascent center)
        ;; ("Org" "~/.emacs.d/resources/icon/Org.xpm" nil nil :ascent center)
        ("Org" "~/.emacs.d/resources/icon/org.xpm" nil nil :ascent center)
        ("Writing" "~/.emacs.d/resources/icon/todo.xpm" nil nil :ascent center)
        ;; ("Emacs" "~/.emacs.d/resources/icon/Emacs.xpm" nil nil :ascent center)
        ("Emacs" "~/.emacs.d/resources/icon/emacs.xpm" nil nil :ascent center)
        ("Code" "~/.emacs.d/resources/icon/code.xpm" nil nil :ascent center)
        ("Programming" "~/.emacs.d/resources/icon/code.xpm" nil nil :ascent center)
        ("Bug" "~/.emacs.d/resources/icon/vcs.xpm" nil nil :ascent center)
        ("Issue" "~/.emacs.d/resources/icon/vcs.xpm" nil nil :ascent center)
        ("Feature" "~/.emacs.d/resources/icon/vcs.xpm" nil nil :ascent center)
        ("VCS" "~/.emacs.d/resources/icon/vcs.xpm" nil nil :ascent center)
        ;; ("Git" "~/.emacs.d/resources/icon/Git.xpm" nil nil :ascent center)
        ("Git" "~/.emacs.d/resources/icon/vcs.xpm" nil nil :ascent center)
        ("Design" "~/.emacs.d/resources/icon/code.xpm" nil nil :ascent center)
        ("Hardware"  "~/.emacs.d/resources/icon/hardware.xpm" nil nil :ascent center)
        ("Audio"  "~/.emacs.d/resources/icon/audio.xpm" nil nil :ascent center)
        ("Analysis"  "~/.emacs.d/resources/icon/analysis.xpm" nil nil :ascent center)
        ("Email" "~/.emacs.d/resources/icon/Email.xpm" nil nil :ascent center)
        ("Idea" "~/.emacs.d/resources/icon/Idea.xpm" nil nil :ascent center)
        ("Project"  "~/.emacs.d/resources/icon/code.xpm" nil nil :ascent center)
        ("Startup"  "~/.emacs.d/resources/icon/Startup.xpm" nil nil :ascent center)
        ("Hack"  "~/.emacs.d/resources/icon/Hack.xpm" nil nil :ascent center)
        ("Anonymous"  "~/.emacs.d/resources/icon/Anonymous.xpm" nil nil :ascent center)
        ("Security"  "~/.emacs.d/resources/icon/Security.xpm" nil nil :ascent center)
        ("Daily" '(space . (:width (16)))) ; to display a 16px horizontal space
        ("Learning"  "~/.emacs.d/resources/icon/org.xpm" nil nil :ascent center)
        ("Linux" "~/.emacs.d/resources/icon/Linux.xpm" nil nil :ascent center)
        ("Config" "~/.emacs.d/resources/icon/config.xpm" nil nil :ascent center)
        ("Command" "~/.emacs.d/resources/icon/command.xpm" nil nil :ascent center)
        ("Document" "~/.emacs.d/resources/icon/document.xpm" nil nil :ascent center)
        ("Info" "~/.emacs.d/resources/icon/Info.xpm" nil nil :ascent center)
        ("GNU" "~/.emacs.d/resources/icon/GNU.xpm" nil nil :ascent center)
        ("Arch" "~/.emacs.d/resources/icon/Arch.xpm" nil nil :ascent center)
        ("Ubuntu" "~/.emacs.d/resources/icon/Ubuntu.xpm" nil nil :ascent center)
        ("Kali" "~/.emacs.d/resources/icon/Kali.xpm" nil nil :ascent center)
        ("BSD" "~/.emacs.d/resources/icon/BSD.xpm" nil nil :ascent center)
        ("Android" "~/.emacs.d/resources/icon/Android.xpm" nil nil :ascent center)
        ("iOS" "~/.emacs.d/resources/icon/iOS.xpm" nil nil :ascent center)
        ("Lisp" "~/.emacs.d/resources/icon/lisp.xpm" nil nil :ascent center)
        ("Common Lisp" "~/.emacs.d/resources/icon/Common-Lisp.xpm" nil nil :ascent center)
        ("Clojure" "~/.emacs.d/resources/icon/Clojure.xpm" nil nil :ascent center)
        ("Ruby" "~/.emacs.d/resources/icon/Ruby.xpm" nil nil :ascent center)
        ("Rails" "~/.emacs.d/resources/icon/Rails.xpm" nil nil :ascent center)
        ("Python" "~/.emacs.d/resources/icon/Python.xpm" nil nil :ascent center)
        ("Perl" "~/.emacs.d/resources/icon/Perl.xpm" nil nil :ascent center)
        ("Shell" "~/.emacs.d/resources/icon/Shell.xpm" nil nil :ascent center)
        ("PHP" "~/.emacs.d/resources/icon/PHP.xpm" nil nil :ascent center)
        ("Haskell" "~/.emacs.d/resources/icon/Haskell.xpm" nil nil :ascent center)
        ("Erlang" "~/.emacs.d/resources/icon/Erlang.xpm" nil nil :ascent center)
        ("Prolog" "~/.emacs.d/resources/icon/Prolog.xpm" nil nil :ascent center)
        ("Assembly" "~/.emacs.d/resources/icon/Assembly.xpm" nil nil :ascent center)
        ("C" "~/.emacs.d/resources/icon/C.xpm" nil nil :ascent center)
        ("C++" "~/.emacs.d/resources/icon/C++.xpm" nil nil :ascent center)
        ("D" "~/.emacs.d/resources/icon/D.xpm" nil nil :ascent center)
        ("Go" "~/.emacs.d/resources/icon/Go.xpm" nil nil :ascent center)
        ("Swift" "~/.emacs.d/resources/icon/Swift.xpm" nil nil :ascent center)
        ("Rust" "~/.emacs.d/resources/icon/Rust.xpm" nil nil :ascent center)
        ("Scala" "~/.emacs.d/resources/icon/Scala.xpm" nil nil :ascent center)
        ("Java" "~/.emacs.d/resources/icon/Java.xpm" nil nil :ascent center)
        ("HTML" "~/.emacs.d/resources/icon/HTML.xpm" nil nil :ascent center)
        ("CSS" "~/.emacs.d/resources/icon/CSS.xpm" nil nil :ascent center)
        ("HTML5" "~/.emacs.d/resources/icon/HTML5.xpm" nil nil :ascent center)
        ("CSS3" "~/.emacs.d/resources/icon/CSS3.xpm" nil nil :ascent center)
        ("JavaScript" "~/.emacs.d/resources/icon/JavaScript.xpm" nil nil :ascent center)
        ("SQL" "~/.emacs.d/resources/icon/SQL.xpm" nil nil :ascent center)
        ("NoSQL" "~/.emacs.d/resources/icon/NoSQL.xpm" nil nil :ascent center)
        ("NewSQL" "~/.emacs.d/resources/icon/NewSQL.xpm" nil nil :ascent center)
        ("R" "~/.emacs.d/resources/icon/R.xpm" nil nil :ascent center)
        ("Julia" "~/.emacs.d/resources/icon/Julia.xpm" nil nil :ascent center)
        ("Quipper" "~/.emacs.d/resources/icon/Quipper.xpm" nil nil :ascent center)
        ("TeX" "~/.emacs.d/resources/icon/TeX.xpm" nil nil :ascent center)
        ("Web" "~/.emacs.d/resources/icon/web.xpm" nil nil :ascent center)
        ("Network" "~/.emacs.d/resources/icon/network.xpm" nil nil :ascent center)
        ("GitHub" "~/.emacs.d/resources/icon/GitHub Octocat.xpm" nil nil :ascent center)
        ("GFW" "~/.emacs.d/resources/icon/GFW.xpm" nil nil :ascent center)
        (".*" '(space . (:width (16))))
        ))


;; set Org agenda to search in ~/Org directory *recursively*.

(setq org-agenda-files '("~/Org/Work/Work.org"
                         ;; "~/Org/Projects/Freelancer.org"
                         ;; "~/Org/Projects/Finance.org"
                         "~/Org/Projects/Business Ideas.org"
                         "~/Org/Wiki/Business/Startup/My Startup/My Startup.org"
                         "~/Org/Daily.org"
                         "~/Org/Capture/Capture.org"
                         "~/Org/Wiki/Things/Things.org"
                         "~/Org/Tasks.org"
                         "~/Org/Projects/Plans.org"
                         "~/Org/Projects/Code.org"
                         "~/Org/Projects/Computer Todos.org"
                         "~/Org/Projects/Projects.org"
                         "~/Org/Projects/Programming Projects.org" ; BIG!
                         ;; "~/Org/Projects/Maker.org"
                         "~/Org/Wiki/Learning/Learning.org"
                         "~/Org/Wiki/Learning/MyLearningPlan/Learn Programming.org"
                         ;; "~/Org/Wiki/Wiki.org"
                         ;; "~/Org/Contacts/Contacts.org"
                         ))

;; (setq org-agenda-text-search-extra-files '("~/Org/Journal.org" "~/Org/Diary/"))

(setq org-agenda-skip-timestamp-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-scheduled-delay-if-deadline 'post-deadline
      org-agenda-skip-scheduled-if-deadline-is-shown t
      ;; NOTE: org-agenda-ignore-properties '(effort appt stats category)
      org-agenda-tags-todo-honor-ignore-options t
      org-agenda-todo-ignore-timestamp nil ; 'all
      org-agenda-todo-ignore-with-date nil
      org-agenda-todo-ignore-scheduled 'future
      )

(setq org-agenda-scheduled-leaders '("Scheduled: " "%2d days /// "))
(setq org-agenda-show-all-dates t)
(setq org-agenda-show-outline-path t)
;; determines how far in advance items with
;; deadlines will show up in the agenda.
(setq org-deadline-warning-days 14)
;; I work late at night! Extend my current day past midnight.
(setq org-extend-today-until 1)

(setq org-agenda-window-setup 'current-window)

(setq org-agenda-span 'week
      org-agenda-start-on-weekday nil)
;; speedup Org Agenda
(setq org-agenda-dim-blocked-tasks nil
      org-agenda-inhibit-startup nil
      org-agenda-use-tag-inheritance nil)

;; toggle log mode in agenda buffer. show all possible log items.
(setq org-agenda-start-with-log-mode t
      org-agenda-log-mode-items '(closed clock)
      org-agenda-log-mode-add-notes t)

;;; Custom Agenda Commands
(add-to-list 'org-agenda-custom-commands
             '(("d" "Agenda and all TODO's"
                ((agenda "")
                 (alltodo "")))
               ("u" "Urgent tasks"
                ((search "[#A]")
                 (todo "Urgent")
                 (tags "Prepare-Today")))
               ("t" "all todo entries"
                todo ""
                ((org-agenda-buffer-name "*Todo List*")))
               ("s" "Tasks to start in the future/someday."
                todo "SOMEDAY")
               ;; ("c" "Clock"
               ;;  ((agenda "" ((org-agenda-sticky nil)
               ;;               (org-agenda-ndays 1)
               ;;               (org-agenda-span-1)
               ;;               (org-agenda-use-time-grid t)
               ;;               (org-agenda-show-log (quote clockcheck))
               ;;               (org-agenda-clockreport nil)))))
               ("p" "Project process - project, BUG, ISSUE, Features"
                ((todo "project")
                 (todo "BUG")
                 (todo "ISSUE")
                 (todo "Features")))
               ;; used to filter out fragment time tasks.
               ("f" "Fragment time tasks"
                ((tags "fragment")))
               ;; ("i" tags-todo "CATEGORY=\"Task\"")
               ;; ("w" tags-todo "CATEGORY=\"Work\"")
               ))


;;; Tag changes that should be triggered by TODO state changes.
;; [C-c C-x a]
;; (setq org-todo-state-tags-triggers
;;       '(("" ("Task" . t))
;;         ('todo ("" . t))
;;         ('done ("ARCHIVE" . t))
;;         ("DONE" ("ARCHIVE" . t))
;;         ("CANCELLED" ("CANCELLED" . t))
;;         ))


;;; [ Calendar ]

;;;_* iCalendar

(setq org-combined-agenda-icalendar-file "~/Org/Calendar/iCalendar.ics")


;;; [ Notify ]

;; 1.
;;   - show in modeline
;; 2.
;;   - sauron (+alert.el)
;; 3.
;;   - org-notify (from org-clock), (notify-send)
;;     - (org-notify "body")
;;     - (org-show-notification "body")
;;   reference org-clock.el function source code.
;;   - (setq org-show-notification-handler '())
;; 4.
;;   - use function `my-func-notify-send'.
;;     (my-func-notify-send
;;      "Warning" "the end is near"
;;      "/usr/share/icons/test.png" "/usr/share/sounds/beep.ogg")


;;; [ org-notify ]

(require 'org-notify)

(setq org-notify-audible nil)

;; ---------------------------------------------------------
;; List of possible parameters:
;;
;;   :time      Time distance to deadline, when this type of notification shall
;;              start.  It's a string: an integral value (positive or negative)
;;              followed by a unit (s, m, h, d, w, M).
;;   :actions   A function or a list of functions to be called to notify the
;;              user.  Instead of a function name, you can also supply a suffix
;;              of one of the various predefined `org-notify-action-xxx'
;;              functions.
;;
;;   :actions -ding, -notify, -window, -notify/window, -message, -email,
;;
;;   :period    Optional: can be used to repeat the actions periodically.
;;              Same format as :time.
;;   :duration  Some actions use this parameter to specify the duration of the
;;              notification.  It's an integral number in seconds.
;;   :audible   Overwrite the value of `org-notify-audible' for this action.
;; ---------------------------------------------------------

(org-notify-add 'default
                '(:time "1h" :period "2h" :duration 8
                        :actions (-notify/window)
                        :audible t)
                )

(org-notify-start 60)



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


;;; [ org-alert ] -- System notifications of org agenda items.

;; - `org-alert-check'
;; - `org-alert-enable' / `org-alert-disable'

(use-package org-alert
  :config
  (setq alert-default-style 'libnotify)
  
  (org-alert-enable)
  )


(provide 'init-my-org-agenda)

;;; init-my-org-agenda.el ends here
