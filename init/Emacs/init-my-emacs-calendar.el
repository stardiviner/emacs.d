;;; init-my-emacs-calendar.el --- init for Calendar
;;; -*- coding: utf-8 -*-

;;; Commentary:

;;; http://www.emacswiki.org/emacs/CategoryCalendar


;;; Code:

;;; Calendar Mode
;;; Usage:
;;; - [M-x calendar]
;; - holidays: [a]
;; - diary: [i d]
;; - anniversary: [i a]
;;
;;; Diary in calendar
;; Usage:
;; - [M-x calendar]
;;   - "d" -- view diary entries
;;   - "m" -- mark diary entries
;;   - "s" -- show all diary entries
;;   - "id" -- insert diary entry
;;   - "iw" -- insert weekly diary entry
;;   - "im" -- insert monthly diary entry
;;
;;; Appointment in calendar


;; ;; Month
;; (setq calendar-month-name-array
;;       ["January" "February" "March"     "April"   "May"      "June"
;;        "July"    "August"   "September" "October" "November" "December"])
;; ;; Week days
;; (setq calendar-day-name-array
;;       ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"])

;;; Calendar Localization
;;; display the ‘celestial-stem’ (天干) and the ‘terrestrial-branch’ (地支) in Chinese:
;; ;; First day of the week
(setq calendar-week-start-day 1) ; 0:Sunday, 1:Monday
(setq calendar-date-style 'american)    ; 'american: month/day/year, 'european: day/month/year, 'iso: year/month/day


;; (setq chinese-calendar-celestial-stem
;;       ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"]
;;       chinese-calendar-terrestrial-branch
;;       ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"])
;; ;;; Localized National Holidays
;; ;;; cal-china-x.el -- Chinese calendar extras
;; (load "~/.emacs.d/init/extensions/cal-china-x.el")
;; (require 'cal-china-x)


;; location
(setq calendar-location-name "Shaoxing")


;; for predicate lunar eclipses.
;; Zhejiang, China Area: Latitude: 27° 09' ~ 31° 11' N , Longitude: 118° 02' ~ 122° 57' E
;; Shaoxing Area: Latitude: 29° 42' ~ 30° 19' 15" , Longitude: 120° 16' 55" ~ 120° 46' 39"
(setq calendar-latitude +30.10)
(setq calendar-longitude +120.40)

(setq calendar-mark-holidays-flag nil
      calendar-mark-diary-entries-flag t
      calendar-view-diary-initially-flag t)

;; TODO how to mark a diary date entry in calendar ?
;; - different font
;; - different color
;; - append + at the day entry in calendar
(setq calendar-today-marker 'calendar-today
      mark-diary-entries-in-calendar t) ; 'calendar-today, "=", face


;;; Calendar Printing
;;; Usage:
;;; - press "t m" on the current month in Calendar Mode buffer. This will create a new buffer named 'calendar.tex' containing a latex representation of the current month.
;;; - then press [C-c C-c] to "compile" the file. You can see it by press [C-c C-c] again.
;;; - press [C-c C-p] to print the compiled PDF file.
;;; Printing to A4 pages
;;; If you print A4 pages, the calendars are probably too wide for you. I often print monthly calendars. The default setup prints them 18cm wide; I want them 17cm wide. Here’s how to do it:
(add-hook 'cal-tex-hook 'my-calendar-print-a4)
(defun my-calendar-print-a4 ()
  "Replace all occurences of 18cm with 17cm."
  (goto-char (point-min))
  (while (search-forward "18cm" nil t)
    (replace-match  "17cm")))
;;; Printing Key bindings
;;; - [p C-h] -- list out all key bindings

;;; Key Bindings
;;; - [C-SPC] + [M-=] -- count the number of days in region (between the mark and the current point).
;;; - [8 C-n] -- move ahead 8 days.

(add-hook 'calendar-initial-window-hook (lambda ()
                                          (calendar-mark-today)
                                          (calendar-mark-holidays)
                                          ;; (mark-diary-entries) ; FIXME: void function.
                                          ))

;; [ Diary ]
;; Diary Mode
;; Usage:
;; - [M-x calendar RET d]
;; - [M-x diary]
;;
;; fancy display
(setq view-diary-entries-initially t
      mark-diary-entries-in-calendar t
      number-of-diary-entries 7)
(add-hook 'diary-display-hook 'fancy-diary-display)
;;(add-hook 'today-visible-calendar-hook 'calendar-mark-today)
;;; highlight color free days in calendar
;;; TODO This does not work
;; (defface my-calendar-free-day-face nil nil)
;; (set-face-background 'my-calendar-free-day-face "green")

;; (defun mycal-hightlight-free-day (date &optional face)
;;   "Highlight DATE with FACE if DATE is free-day.
;;     Default face is `mycal-freeday-face'."
;;   (when (and (calendar-date-is-visible-p date)
;;              (member (calendar-day-of-week date) '(0 6)))
;;     (save-excursion
;;       (calendar-goto-date date)
;;       (if (fboundp 'set-extent-properties)
;;           (set-extent-properties (make-extent (1- (point)) (1+ (point)))
;;                                  (list 'face (or face 'mycal-freeday-face)
;;                                        'priority -1))
;;         (set-text-properties (1- (point)) (1+ (point))
;;                              (list 'face (or face 'mycal-freeday-face)
;;                                    'priority -1))))
;;     ))

;; (defun mycal-mark-freedays ()
;;   "Scan Calendar buffer and highlight freedays.
;;     Prefix ARG specifies number of weeks to highlight."
;;   (interactive)

;;   (save-excursion
;;     (beginning-of-buffer)

;;     ;; Process first week
;;     (calendar-end-of-week 1)
;;     (let* ((date (calendar-cursor-to-nearest-date))
;;            (day (car (cdr date))))
;;       (when (> day 7)
;;         (calendar-backward-week 1))
;;       ;; Refresh date and day
;;       (setq date (calendar-cursor-to-nearest-date))
;;       (setq day (car (cdr date)))
;;       (mycal-hightlight-free-day date)

;;       (unless (= day 1)
;;         (calendar-backward-day 1)
;;         (mycal-hightlight-free-day (calendar-cursor-to-nearest-date))
;;         (calendar-end-of-week 1)))

;;     ;; Process other monthes
;;     (let* ((date (calendar-cursor-to-nearest-date))
;;            (absdate (+ 6 (calendar-absolute-from-gregorian date)))
;;            (gregd (calendar-gregorian-from-absolute absdate)))
;;       (while (calendar-date-is-visible-p gregd)
;;         (mycal-hightlight-free-day gregd)

;;         (setq gregd (calendar-gregorian-from-absolute (+ absdate 1)))
;;         (mycal-hightlight-free-day gregd)

;;         (setq absdate (+ 7 absdate))
;;         (setq gregd (calendar-gregorian-from-absolute absdate))))
;;     ))

;; (defadvice mark-calendar-holidays (after highlight-free-days activate)
;;   "Highlight freedays as well."
;;   (mycal-mark-freedays))


;;; [ icalendar ]



;;; [ calfw ] -- Calendar framework for Emacs
;;; https://github.com/kiwanami/emacs-calfw
;; Features:
;; * holidays
;;    get the holidays using the function `calendar-holiday-list`. see `holidays.el`
;; Usage:
;; - [SPC] -- Pushing SPC key, the detail buffer pops up. Pushing SPC key again, the buffer is closed.
;; - command `cfw:open-calendar-buffer`
;; Key bindings (in calfw buffer):
;; - [TAB] -- navigate to next item.
;; - [SPC] -- show details.
;; - [RET] -- Jump (howm, orgmode)
;; - [.]   -- goto today.
;; - [D]   -- day view.
;; - [W]   -- week view.
;; - [T]   -- two weeks view.
;; - [M]   -- month view.
;; - [h/j/k/l] / [b/f/n/p] -- navigate [back/forward/up/down]
;; - [^/$] -- navigate to week [begin/end]
;; - [q]   -- quit
;; - [r]   -- refresh
;; - [t]   -- to to a day.
;; - [C-v] -- next month.
;; - [M-v] -- previous month.

(require 'calfw)

;;; Holidays
;;; holidays.el
;; calfw collects holidays from the customize variable `calendar-holidays` which belongs to `holidays.el` in Emacs.
;; calfw gets the holidays using the function (calendar-holiday-list).
(require 'holidays)

(setq general-holidays nil) ; get rid of U.S. holidays
(setq christian-holidays nil) ; get rid of christan holidays
(setq view-calendar-holidays-initially t)


;;; Annotations
;;; variable -> :annotation-sources

;;; for Org-mode
;;; Usage:
;;; - [M-x cfw:open-org-calendar]
(require 'calfw-org)

;;; for iCalendar (Google Calendar) users:
(require 'calfw-ical)
;; TODO (cfw:open-ical-calendar "http://www.google.com/calendar/ical/.../basic.ics")

;;; for Diary users:
;;; Usage:
;;; - [M-x cfw:open-diary-calendar]
(require 'calfw-cal)

;;; calfw-gcal.el -- edit Google Calendar for calfw.
;; (require 'calfw-gcal)

;;; for Howm users:
;; (require 'calfw-howm)


;;; General setting
(defun my-open-calfw ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources (list
                      (cfw:org-create-source "CadetBlue4") ; Org-mode source
                      (cfw:cal-create-source "orange") ; Diary source
                      ;; TODO
                      ;; (cfw:ical-create-source "Moon" "~/moon.ics" "Gray") ; iCalendar source1
                      ;; (cfw:ical-create-source "gcal" "https://..../basic.ics" "IndianRed") ; Google Calendar ICS
                      ;; (cfw:howm-create-source "blue") ; howm source
                      )
   ;; TODO add annotation-sources
   :annotation-sources (list
                        (cfw:ical-create-source "Moon" "~/moon.ics" "Gray") ; Moon annotations
                        )
   :view 'month                         ; 'month, 'week, 'day
   ))

;;; Global Keybinding
(unless (boundp 'my-calendar-prefix-map)
  (define-prefix-command 'my-calendar-prefix-map))
(define-key my-tools-prefix-map (kbd "c") 'my-calendar-prefix-map)

(define-key my-calendar-prefix-map (kbd "c") 'my-open-calfw)
(define-key my-calendar-prefix-map (kbd "x") 'cfw:open-calendar-buffer)


;;; Faces
;;; TODO change those faces colors.
;; (custom-set-faces
(set-face-attribute 'cfw:face-title nil
                    :foreground "#f0dfaf"
                    :weight 'bold :height 2.5)
;; week (1-5)
(set-face-attribute 'cfw:face-header nil
                    :foreground "sky blue"
                    :weight 'bold)
;; saturday
(set-face-attribute 'cfw:face-saturday nil
                    :foreground "orange" :background "#333333"
                    :weight 'bold
                    ;; :box '(:color "orange" :line-width -1)
                    )
;; sunday
(set-face-attribute 'cfw:face-sunday nil
                    :foreground "orange" :background "#333333"
                    :weight 'bold
                    ;; :box '(:color "yellow" :line-width -1)
                    )
;; holidays
(set-face-attribute 'cfw:face-holiday nil
                    :foreground "yellow"
                    :weight 'bold
                    :box '(:color "yellow" :line-width -1))
;; grid
(set-face-attribute 'cfw:face-grid nil
                    :foreground "#444444"
                    )
;; ??
(set-face-attribute 'cfw:face-default-content nil
                    :foreground "gray")
;; ??
(set-face-attribute 'cfw:face-periods nil
                    :foreground "cyan")
;; ??
(set-face-attribute 'cfw:face-annotation nil
                    :foreground "dark green")
;; ??
(set-face-attribute 'cfw:face-disable nil
                    :foreground "#333333"
                    :strike-through t)
;; 1-31 (N)
(set-face-attribute 'cfw:face-day-title nil
                    :foreground "dark gray")
;; 1-31
(set-face-attribute 'cfw:face-default-day nil
                    :inherit 'cfw:face-day-title
                    :foreground "gray"
                    :weight 'bold
                    )
;; today (N)
(set-face-attribute 'cfw:face-today-title nil
                    :foreground "cyan" :background "black"
                    :weight 'bold
                    :box '(:color "dark cyan" :line-width -1))
;; events at today.
(set-face-attribute 'cfw:face-today nil
                    :foreground "dark cyan" :background " "
                    :weight 'normal
                    ;; :underline '(:color "#333333")
                    )
;; current select
(set-face-attribute 'cfw:face-select nil
                    :foreground "white" :background "dark cyan"
                    :box '(:color "cyan" :line-width -1))
;; toolbar [ < ] [ > ] [Today ]                      [Day] [Week] [Two Weeks] [Month]
(set-face-attribute 'cfw:face-toolbar nil
                    :background " ")
(set-face-attribute 'cfw:face-toolbar-button-on nil
                    :foreground "white"
                    :weight 'bold)
(set-face-attribute 'cfw:face-toolbar-button-off nil
                    :foreground "dark gray"
                    :weight 'normal)


;; Grid frame
;; Default setting
(setq cfw:fchar-junction ?+
      cfw:fchar-vertical-line ?|
      cfw:fchar-horizontal-line ?-
      cfw:fchar-left-junction ?+
      cfw:fchar-right-junction ?+
      cfw:fchar-top-junction ?+
      cfw:fchar-top-left-corner ?+
      cfw:fchar-top-right-corner ?+ )

;; Unicode characters
(setq cfw:fchar-junction ?╋
      cfw:fchar-vertical-line ?┃
      cfw:fchar-horizontal-line ?━
      cfw:fchar-left-junction ?┣
      cfw:fchar-right-junction ?┫
      cfw:fchar-top-junction ?┯
      cfw:fchar-top-left-corner ?┏
      cfw:fchar-top-right-corner ?┓)

;; Another unicode chars
(setq cfw:fchar-junction ?╬
      cfw:fchar-vertical-line ?║
      cfw:fchar-horizontal-line ?═
      cfw:fchar-left-junction ?╠
      cfw:fchar-right-junction ?╣
      cfw:fchar-top-junction ?╦
      cfw:fchar-top-left-corner ?╔
      cfw:fchar-top-right-corner ?╗)

;;; Line breaking
(setq cfw:render-line-breaker 'cfw:render-line-breaker-wordwrap)
;;; 'cfw:render-line-breaker-none
;;;     Never breaks lines. Longer contents are truncated.
;;; 'cfw:render-line-breaker-simple (default)
;;;     This strategy breaks lines with rigid width. This may be not so beautiful, but In the most cases it looks good.
;;; 'cfw:render-line-breaker-wordwrap
;;;     This strategy breaks lines with the emacs function 'fill-region'. Although, the line breaking algorithm of the Emacs is not so smart as more complicated ones, such as Knuth/Plass algorithm, this strategy is better than the simple one.

;;; Calfw framework details
;;; How to add a new calendar source?
;;; Defining the 'cfw:source' object, one can extend calfw calendar source.
;;; 'cfw:source-data' details


;;; cfw:source-data


;;; keybindings
;; ;; Vim style navigation around
;; (define-key cfw:calendar-mode-map (kbd "k") 'cfw:navi-previous-week-command)
;; (define-key cfw:calendar-mode-map (kbd "j") 'cfw:navi-next-week-command)
;; (define-key cfw:calendar-mode-map (kbd "h") 'cfw:navi-previous-day-command)
;; (define-key cfw:calendar-mode-map (kbd "l") 'cfw:navi-next-day-command)
;; ;; 
;; (define-key cfw:calendar-mode-map (kbd "p") 'cfw:navi-previous-week-command)
;; (define-key cfw:calendar-mode-map (kbd "n") 'cfw:navi-next-week-command)
;; (define-key cfw:calendar-mode-map (kbd "b") 'cfw:navi-previous-day-command)
;; (define-key cfw:calendar-mode-map (kbd "f") 'cfw:navi-next-day-command)


;; [ Appointment & Remind ]
;; (require 'appt)
;; (setq appt-issue-message t) ; raise issue message for appointment

;; planner




(provide 'init-my-emacs-calendar)

;;; init-my-emacs-calendar.el ends here
