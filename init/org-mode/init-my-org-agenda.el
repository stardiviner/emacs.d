;;; init-my-org-agenda.el --- init for Org Agenda
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(require 'org-agenda)

(setq org-agenda-window-setup 'current-window)

;; Agenda Views
(setq org-agenda-align-tags-to-column -100
      org-agenda-tags-column -100)
(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c %? e %?-12t % s")
        (timeline . " % s")
        (effort . " %e %(or (org-entry-get (point) \"Effort\") \"0:00\")")
        (todo . " %i %-12:c")
        (search . " %i %-12:c")
        (tags . " %i %-12:c")
        ))
(setq org-agenda-scheduled-leaders '("Scheduled: " "%3d days | "))

(setq org-agenda-block-separator ?=
      org-agenda-compact-blocks t)

(setq org-agenda-prefer-last-repeat t)

;;; sorting strategy
(setq org-agenda-sorting-strategy
      '((agenda time-up deadline-up priority-down ts-up habit-down category-keep)
        (todo priority-down category-keep)
        (tags priority-down category-keep)
        (search category-keep))
      org-agenda-sorting-strategy-selected
      '(time-up priority-down deadline-up ts-up habit-down category-keep)
      )

;;; Time Grid
(setq org-agenda-timegrid-use-ampm t)

;;; specify different color for days
(defun my-org-agenda-get-day-face-fn (date)
  "Return the face DATE should be displayed with."
  (let ((day-of-week (calendar-day-of-week date)))
    (cond
     ((or (= day-of-week 1) (= day-of-week 5))
      '(:foreground "forest green" :box (:color "dim gray" :line-width 3)))
     ((org-agenda-todayp date)
      'org-agenda-date-today)
     ((member day-of-week org-agenda-weekend-days)
      'org-agenda-date-weekend)
     (t 'org-agenda-date))))

(setq org-agenda-day-face-function 'my-org-agenda-get-day-face-fn)


;; include `diary-file' from `calendar'
(setq org-agenda-include-diary nil ; speed up Org Agenda generation.
      ;; diary-file
      org-agenda-diary-file 'diary-file
      ;; org-agenda-insert-diary-strategy 'date-tree
      )

;;; Icon

(use-package all-the-icons
  :ensure t
  :config
  (setq org-agenda-category-icon-alist
        `(("Diary" ,(list (all-the-icons-faicon "file-text-o")) nil nil :ascent center)
          ("Todo" ,(list (all-the-icons-faicon "check-square-o" :height 1.2)) nil nil :ascent center)
          ("Habit" ,(list (all-the-icons-faicon "refresh")) nil nil :ascent center)
          ("Star" ,(list (all-the-icons-faicon "star-o")) nil nil :ascent center)
          ("Org" ,(list (all-the-icons-fileicon "org")) nil nil :ascent center)
          
          ;; <Work>
          ("Work" ,(list (all-the-icons-faicon "black-tie")) nil nil :ascent center)
          ("Writing" ,(list (all-the-icons-faicon "pencil-square-o" :height 1.1)) nil nil :ascent center)
          ("Print" ,(list (all-the-icons-faicon "print")) nil nil :ascent center)

          ;; <Programming>
          ("Emacs" ,(list (all-the-icons-fileicon "emacs")) nil nil :ascent center)
          ("Code" ,(list (all-the-icons-faicon "keyboard-o")) nil nil :ascent center) ; "file-code-o"
          ("Programming" ,(list (all-the-icons-faicon "code")) nil nil :ascent center)
          ("Bug" ,(list (all-the-icons-faicon "bug" :height 1.1)) nil nil :ascent center)
          ("Issue" ,(list (all-the-icons-octicon "issue-opened" :height 1.2)) nil nil :ascent center)
          ("Feature" ,(list (all-the-icons-faicon "check-circle-o" :height 1.2)) nil nil :ascent center)
          ("VCS" ,(list (all-the-icons-faicon "git")) nil nil :ascent center)
          ("Git" ,(list (all-the-icons-faicon "git")) nil nil :ascent center)
          ("Database" ,(list (all-the-icons-faicon "database" :height 1.2)) nil nil :ascent center)
          ("Design" ,(list (all-the-icons-material "palette")) nil nil :ascent center)
          ("Computer" ,(list (all-the-icons-faicon "laptop")) nil nil :ascent center) ; desktop
          ("Laptop" ,(list (all-the-icons-faicon "laptop")) nil nil :ascent center)
          ("Hardware" ,(list (all-the-icons-faicon "desktop")) nil nil :ascent center)
          ("Server" ,(list (all-the-icons-faicon "server")) nil nil :ascent center)
          ("Audio" ,(list (all-the-icons-faicon "file-audio-o")) nil nil :ascent center)
          ("Analysis" ,(list (all-the-icons-faicon "bar-chart" :height 0.9)) nil nil :ascent center)
          ("Email" ,(list (all-the-icons-material "email")) nil nil :ascent center)
          ("Idea" ,(list (all-the-icons-faicon "lightbulb-o" :height 1.2)) nil nil :ascent center)
          ("Project" ,(list (all-the-icons-faicon "tasks" :height 1.1)) nil nil :ascent center)
          ("Agriculture" ,(list (all-the-icons-faicon "leaf" :height 1.1)) nil nil :ascent center)
          ("Industry" ,(list (all-the-icons-faicon "industry")) nil nil :ascent center)
          ("Express" ,(list (all-the-icons-faicon "truck")) nil nil :ascent center)
          ("Startup" ,(list (all-the-icons-faicon "codepen")) nil nil :ascent center)
          ("Hack" ,(list (all-the-icons-material "security")) nil nil :ascent center)
          ("Crack" ,(list (all-the-icons-faicon "user-secret" :height 1.1)) nil nil :ascent center)
          ("Security" ,(list (all-the-icons-material "security")) nil nil :ascent center)
          ;; ("Anonymous"  "~/.emacs.d/resources/icon/Anonymous.xpm" nil nil :ascent center)
          ("Daily" ,(list (all-the-icons-faicon "calendar-check-o")) nil nil :ascent center)
          ("Learning" ,(list (all-the-icons-material "library_books")) nil nil :ascent center)
          ("University" ,(list (all-the-icons-faicon "university" :height 0.9)) nil nil :ascent center)
          ("Reading" ,(list (all-the-icons-faicon "book")) nil nil :ascent center)
          ("Linux" ,(list (all-the-icons-faicon "linux" :height 1.2)) nil nil :ascent center)
          ("macOS" ,(list (all-the-icons-faicon "apple")) nil nil :ascent center)
          ("Windows" ,(list (all-the-icons-faicon "windows")) nil nil :ascent center)
          ("Config" ,(list (all-the-icons-faicon "cogs")) nil nil :ascent center)
          ("Command" ,(list (all-the-icons-faicon "terminal")) nil nil :ascent center)
          ("Document" ,(list (all-the-icons-faicon "file-o")) nil nil :ascent center)
          ("Info" ,(list (all-the-icons-faicon "info")) nil nil :ascent center)
          ;; ("GNU" ,(list (all-the-icons-faicon "")) nil nil :ascent center)
          ;; ("Arch" ,(list (all-the-icons-faicon "arch-linux")) nil nil :ascent center)
          ;; ("Ubuntu" ,(list (all-the-icons-faicon "ubuntu-linux")) nil nil :ascent center)
          ;; ("BSD" ,(list (all-the-icons-faicon "bsd")) nil nil :ascent center)
          ("Android" ,(list (all-the-icons-faicon "android")) nil nil :ascent center)
          ("Apple" ,(list (all-the-icons-faicon "apple")) nil nil :ascent center)
          ("Lisp" ,(list (all-the-icons-fileicon "lisp")) nil nil :ascent center)
          ("Common Lisp" ,(list (all-the-icons-fileicon "clisp")) nil nil :ascent center)
          ("Clojure" ,(list (all-the-icons-alltheicon "clojure-line")) nil nil :ascent center)
          ("CLJS" ,(list (all-the-icons-fileicon "cljs")) nil nil :ascent center)
          ("Ruby" ,(list (all-the-icons-alltheicon "ruby")) nil nil :ascent center)
          ("Python" ,(list (all-the-icons-alltheicon "python")) nil nil :ascent center)
          ("Perl" ,(list (all-the-icons-alltheicon "perl")) nil nil :ascent center)
          ("Shell" ,(list (all-the-icons-faicon "terminal")) nil nil :ascent center)
          ("PHP" ,(list (all-the-icons-fileicon "php")) nil nil :ascent center)
          ("Haskell" ,(list (all-the-icons-alltheicon "haskell")) nil nil :ascent center)
          ("Erlang" ,(list (all-the-icons-alltheicon "erlang")) nil nil :ascent center)
          ("Prolog" ,(list (all-the-icons-alltheicon "prolog")) nil nil :ascent center)
          ;; ("Assembly" ,(list (all-the-icons-alltheicon "")) nil nil :ascent center)
          ("C Language" ,(list (all-the-icons-alltheicon "c")) nil nil :ascent center)
          ("C++ Language" ,(list (all-the-icons-alltheicon "cplusplus")) nil nil :ascent center)
          ("Go Language" ,(list (all-the-icons-alltheicon "go")) nil nil :ascent center)
          ("Swift" ,(list (all-the-icons-alltheicon "swift")) nil nil :ascent center)
          ("Rust" ,(list (all-the-icons-alltheicon "rust")) nil nil :ascent center)
          ("JavaScript" ,(list (all-the-icons-alltheicon "javascript" :height 1.1)) nil nil :ascent center)
          ("Java" ,(list (all-the-icons-alltheicon "java")) nil nil :ascent center)
          ("HTML5" ,(list (all-the-icons-alltheicon "html5")) nil nil :ascent center)
          ("HTML" ,(list (all-the-icons-alltheicon "html5")) nil nil :ascent center)
          ("CSS3" ,(list (all-the-icons-alltheicon "css3")) nil nil :ascent center)
          ("CSS" ,(list (all-the-icons-alltheicon "css3")) nil nil :ascent center)
          ("SQL" ,(list (all-the-icons-faicon "database")) nil nil :ascent center)
          ("PostgreSQL" ,(list (all-the-icons-alltheicon "postgresql")) nil nil :ascent center)
          ("R" ,(list (all-the-icons-fileicon "R")) nil nil :ascent center)
          ("Julia" ,(list (all-the-icons-fileicon "julia")) nil nil :ascent center)
          ("TeX" ,(list (all-the-icons-fileicon "tex")) nil nil :ascent center)
          ("LaTeX" ,(list (all-the-icons-fileicon "tex")) nil nil :ascent center)
          ("Web" ,(list (all-the-icons-faicon "globe" :height 1.1)) nil nil :ascent center)
          ("Network" ,(list (all-the-icons-faicon "sitemap")) nil nil :ascent center)
          ("GitHub" ,(list (all-the-icons-faicon "github")) nil nil :ascent center)
          ("Bitbucket" ,(list (all-the-icons-faicon "bitbucket")) nil nil :ascent center)
          ("Bitcoin" ,(list (all-the-icons-faicon "btc")) nil nil :ascent center)
          ;; ("GFW" "~/.emacs.d/resources/icon/GFW.xpm" nil nil :ascent center)

          ;; <Design>
          ("Design" ,(list (all-the-icons-faicon "paint-brush")) nil nil :ascent center)
          
          ;; <Life>
          ("Home" ,(list (all-the-icons-material "home" :height 1.1)) nil nil :ascent center)
          ("Hotel" ,(list (all-the-icons-material "hotel")) nil nil :ascent center)
          ("Entertainment" ,(list (all-the-icons-faicon "youtube")) nil nil :ascent center)
          ("Place" ,(list (all-the-icons-material "place")) nil nil :ascent center)
          ("Health" ,(list (all-the-icons-faicon "medkit" :height 1.1)) nil nil :ascent center)
          ("Hospital" ,(list (all-the-icons-faicon "hospital-o" :height 1.3)) nil nil :ascent center)
          ("Dining" ,(list (all-the-icons-faicon "cutlery")) nil nil :ascent center)
          ("Shopping" ,(list (all-the-icons-faicon "shopping-basket")) nil nil :ascent center)
          ("Express" ,(list (all-the-icons-material "local_shipping")) nil nil :ascent center)
          ("Sport" ,(list (all-the-icons-faicon "dribbble")) nil nil :ascent center)
          ("Game" ,(list (all-the-icons-faicon "gamepad")) nil nil :ascent center)
          ("Sex" ,(list (all-the-icons-faicon "female" :height 1.2)) nil nil :ascent center)
          ("News" ,(list (all-the-icons-faicon "newspaper-o")) nil nil :ascent center)
          ("Car" ,(list (all-the-icons-faicon "car")) nil nil :ascent center)
          ("Bus" ,(list (all-the-icons-faicon "bus")) nil nil :ascent center)
          ("Contact" ,(list (all-the-icons-material "contact_mail")) nil nil :ascent center)
          ("Talk" ,(list (all-the-icons-faicon "comments" :height 1.1)) nil nil :ascent center)
          ("Video-Call" ,(list (all-the-icons-material "video_call")) nil nil :ascent center)
          ("Call" ,(list (all-the-icons-faicon "phone" :height 1.3)) nil nil :ascent center)
          ("Music" ,(list (all-the-icons-faicon "music")) nil nil :ascent center)
          ("Airplane" ,(list (all-the-icons-faicon "plane")) nil nil :ascent center)
          ("Travel" ,(list (all-the-icons-faicon "motorcycle")) nil nil :ascent center)
          ("Gift" ,(list (all-the-icons-faicon "gift")) nil nil :ascent center)
          ("WiFi" ,(list (all-the-icons-faicon "wifi")) nil nil :ascent center)
          ("Search" ,(list (all-the-icons-faicon "search" :height 1.2)) nil nil :ascent center)
          ("Mobile" ,(list (all-the-icons-material "tablet_mac" :height 1.1)) nil nil :ascent center)
          ("WeChat" ,(list (all-the-icons-faicon "weixin")) nil nil :ascent center)
          ("QQ" ,(list (all-the-icons-faicon "qq" :height 1.1)) nil nil :ascent center)
          ("Weibo" ,(list (all-the-icons-faicon "weibo")) nil nil :ascent center)
          ("Slack" ,(list (all-the-icons-faicon "slack")) nil nil :ascent center)
          ("Facebook" ,(list (all-the-icons-faicon "facebook-official")) nil nil :ascent center)
          ("Twitter" ,(list (all-the-icons-faicon "twitter-square")) nil nil :ascent center)
          ("YouTube" ,(list (all-the-icons-faicon "youtube-square")) nil nil :ascent center)
          ("RSS" ,(list (all-the-icons-faicon "rss-square")) nil nil :ascent center)
          ("Wikipedia" ,(list (all-the-icons-faicon "wikipedia-w")) nil nil :ascent center)
          ("Money" ,(list (all-the-icons-faicon "usd")) nil nil :ascent center)
          ("Accounting" ,(list (all-the-icons-faicon "pie-chart")) nil nil :ascent center)
          ("Bank" ,(list (all-the-icons-material "account_balance")) nil nil :ascent center)
          ("Person" ,(list (all-the-icons-faicon "male")) nil nil :ascent center)
          ("Birthday" ,(list (all-the-icons-faicon "birthday-cake")) nil nil :ascent center)
          
          ;; <Business>
          ("Calculate" ,(list (all-the-icons-faicon "percent")) nil nil :ascent center)
          ("Chart" ,(list (all-the-icons-faicon "bar-chart")) nil nil :ascent center)
          
          ;; <Science>
          ("Chemistry" ,(list (all-the-icons-faicon "flask")) nil nil :ascent center)
          ("Language" ,(list (all-the-icons-faicon "language")) nil nil :ascent center)
          
          (".*" ,(list (all-the-icons-faicon "question-circle-o")) nil nil :ascent center)
          ;; (".*" '(space . (:width (16))))
          ))
  )


(setq org-agenda-files
      (delq nil
            (mapcar
             (lambda (f) (and (file-exists-p f) f))
             '("~/Org/Wiki/Business/Startup/My Startup/My Startup.org"
               "~/Org/Wiki/Things/Things.org" ; Buy Things
               "~/Org/Tasks/"
               "~/Org/Tasks/Work Tasks/"
               "~/Org/Tasks/Family Tasks/"
               "~/Org/Tasks/Travel/"
               "~/Org/Programming/"
               "~/Org/Projects/"
               "~/Org/Projects/Agriculture Projects/"
               "~/Org/Projects/Programming Projects/"
               "~/Org/Projects/Writing Projects/"
               "~/Org/Projects/Business Projects/"
               "~/Org/Projects/Organization/"
               "~/Org/Projects/Interpersonal Network/"
               "~/Org/Learning Plan/"
               ;; "~/Org/Contacts/Contacts.org"
               "~/Org/Calendars/Anniversary.org"
               "~/Org/Myself/"
               ))))

(setq org-agenda-text-search-extra-files '(agenda-archives "~/Org/Diary/"))

(setq org-agenda-skip-timestamp-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-scheduled-delay-if-deadline 'post-deadline
      org-agenda-skip-scheduled-if-deadline-is-shown t
      org-deadline-past-days 3
      ;; XXX: org-agenda-ignore-properties '(effort appt stats category)
      org-agenda-tags-todo-honor-ignore-options t
      )

;;; Org-Agenda All Todo List
(setq org-agenda-todo-ignore-timestamp 'all
      org-agenda-todo-ignore-with-date nil
      org-agenda-todo-ignore-scheduled 'future
      )

(setq org-agenda-span 'day)
;; speedup Org Agenda
(setq org-agenda-inhibit-startup t
      org-agenda-dim-blocked-tasks nil ; don't dim blocked tasks: past deadline, etc
      org-agenda-use-tag-inheritance nil)

;;; toggle log mode in agenda buffer. Press [l] in org-agenda buffer.
(setq org-agenda-start-with-log-mode '(closed clock)
      org-agenda-log-mode-items '(closed clock))


;;; Tag changes that should be triggered by TODO state changes.
;; [C-c C-x a]
;; (setq org-todo-state-tags-triggers
;;       '(("" ("Task" . t))
;;         ('todo ("" . t))
;;         ('done ("ARCHIVE" . t))
;;         ("DONE" ("ARCHIVE" . t))
;;         ("CANCELLED" ("CANCELLED" . t))
;;         ))


;;; [ Composite Agenda View ]
;;; Usage: `(org-agenda nil "c")'

(defun org-agenda-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(defun org-agenda-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(add-to-list 'org-agenda-custom-commands
             '("c" "Custom Agenda with in progress tasks, priority tasks (and all tasks)."
               ((todo "STARTED")
                (todo "INPROGRESS")
                (tags "PRIORITY=\"A\""
                      ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                       (org-agenda-overriding-header "High-priority unfinished tasks:")))
                (agenda ""
                        ((org-agenda-ndays 1)
                         (org-agenda-span 1)
                         (org-agenda-use-time-grid t))
                        )
                ;; (alltodo ""
                ;;          ((org-agenda-skip-function
                ;;            '(or (org-agenda-skip-subtree-if-habit)
                ;;                 (org-agenda-skip-subtree-if-priority ?A)
                ;;                 (org-agenda-skip-if nil '(scheduled deadline))))
                ;;           (org-agenda-overriding-header "All normal priority tasks:")))
                )
               ((org-agenda-compact-blocks t)))
             )

(add-to-list 'org-agenda-custom-commands
             '("F" "[F]uture tasks in someday"
               todo "SOMEDAY"))

(add-to-list 'org-agenda-custom-commands
             '("C" "Tody [C]locked tasks."
               ((agenda ""
                        ((org-agenda-ndays 1)
                         (org-agenda-span-1)
                         (org-agenda-use-time-grid t)
                         (org-agenda-include-diary nil)
                         (org-agenda-show-log (quote clockcheck))
                         (org-agenda-clockreport t))))))

(add-to-list 'org-agenda-custom-commands
             '("p" "[p]rogramming - BUG, ISSUE, FEATURE etc."
               ((todo "BUG")
                (todo "ISSUE")
                (todo "FEATURE"))))

(add-to-list 'org-agenda-custom-commands
             '("w" "[W]ork"
               todo "WORK"
               ((org-agenda-overriding-header "Work"))))

;; used to filter out fragment time tasks.
(add-to-list 'org-agenda-custom-commands
             '("f" "[f]ragment time tasks"
               tags "fragment"
               ((org-agenda-overriding-header "Fragment Tasks"))
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


(define-key org-agenda-mode-map (kbd "M-s") 'org-search-view)


;;;; [ org-review ] -- Track when you have done a review in org mode.

;; (use-package org-review
;;   :ensure t
;;   :config
;;   (add-to-list 'org-agenda-custom-commands
;;                '("R" "Review projects" tags-todo "-CANCELLED/"
;;                  ((org-agenda-overriding-header "Reviews Scheduled")
;;                   (org-agenda-skip-function 'org-review-agenda-skip)
;;                   (org-agenda-cmp-user-defined 'org-review-compare)
;;                   (org-agenda-sorting-strategy '(user-defined-down)))))
;;
;;   (add-to-list 'org-default-properties "NEXT_REVIEW")
;;   (add-to-list 'org-default-properties "LAST_REVIEW")
;;   )

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

(org-notify-start 300)

;;; [ org-pomodoro ] -- adds support for Pomodoro technique in Org-mode.

(use-package org-pomodoro
  :ensure t
  :commands (org-pomodoro)
  :init
  (unless (boundp 'pomodoro-prefix)
    (define-prefix-command 'pomodoro-prefix))
  :bind (:map pomodoro-prefix
              ("o" . org-pomodoro)
              :map Org-prefix
              ("p" . org-pomodoro)
              )
  :config
  (setq org-pomodoro-play-sounds t
        org-pomodoro-start-sound-p t
        org-pomodoro-ticking-sound-p nil
        ;; org-pomodoro-ticking-sound
        org-pomodoro-ticking-sound-args "-volume 50" ; adjust ticking sound volume
        ;; org-pomodoro-start-sound-args "-volume 0.3"
        ;; org-pomodoro-long-break-sound-args "-volume 0.3"
        org-pomodoro-audio-player "/usr/bin/mplayer"
        org-pomodoro-format "Pomodoro: %s" ; mode-line string
        )

  ;; start another pomodoro automatically upon a break end.
  (add-hook 'org-pomodoro-break-finished-hook
            (lambda ()
              (interactive)
              (org-pomodoro '(16)) ; double prefix [C-u C-u]
              ))
  )



(provide 'init-my-org-agenda)

;;; init-my-org-agenda.el ends here
