;;; init-my-tool-calendar.el --- init for Calendar
;;; -*- coding: utf-8 -*-

;;; Commentary:


;;; Code:

(unless (boundp 'calendar-prefix)
  (define-prefix-command 'calendar-prefix))
(define-key tools-prefix (kbd "c") 'calendar-prefix)
(unless (boundp 'Org-prefix)
  (define-prefix-command 'Org-prefix))
(define-key Org-prefix (kbd "C") 'calendar-prefix)


;;; [ calendar ]

(use-package calendar
  :bind (:map calendar-prefix ("c" . calendar))
  :config
  ;; ;; Month
  ;; (setq calendar-month-name-array
  ;;       ["January" "February" "March"     "April"   "May"      "June"
  ;;        "July"    "August"   "September" "October" "November" "December"])
  ;; ;; Week days
  ;; (setq calendar-day-name-array
  ;;       ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"])

  ;; First day of the week
  (setq calendar-week-start-day 1) ; 0:Sunday, 1:Monday
  (setq calendar-weekend-days '(0 6))
  ;; 'american: month/day/year, 'european: day/month/year, 'iso: year/month/day
  (setq calendar-date-style 'american)

  ;; mark holidays
  (setq calendar-mark-holidays-flag t
        calendar-holiday-marker 'holiday)
  ;; (add-hook 'calendar-initial-window-hook 'calendar-mark-holidays) ; this will slow down `org-time-stamp-inactive' performance.

  ;; mark today
  (setq calendar-today-marker 'calendar-today)
  (set-face-attribute 'calendar-today nil
                      :inherit 'highlight
                      :box '(:color "dim gray" :line-width -1 :style nil))
  (add-hook 'calendar-initial-window-hook 'calendar-mark-today)
  
  ;; mark diary entries
  ;; (setq calendar-mark-diary-entries-flag t)

  ;; Annotations
  ;; variable -> :annotation-sources

  
  ;; Calendar Location: Latitude, Longitude.
  ;;
  ;; for predicate lunar eclipses.
  ;;
  ;; Zhejiang, China Area: Latitude: 27° 09' ~ 31° 11' N , Longitude: 118° 02' ~ 122° 57' E
  ;; Shaoxing Area: Latitude: 29° 42' ~ 30° 19' 15" , Longitude: 120° 16' 55" ~ 120° 46' 39"
  ;; Yunnan, Dali: Latitude: 25.60, Longitude: 100.23,

  ;; - Zhejiang, Shaoxing, Zhuji: 29.72, 120.20, UTC+8 (China): +480
  (setq calendar-location-name "Shaoxing Town"
        calendar-time-zone +480
        calendar-latitude 29.72
        calendar-longitude 120.20)
  )

;;; Localized National Holidays
(use-package holidays
  :config
  ;; `calfw' collects holidays from function `calendar-holiday-list' and the
  ;; customize variable `calendar-holidays' which belongs to `holidays.el` in
  ;; Emacs.
  ;; (setq holiday-general-holidays nil) ; get rid of U.S. holidays
  (setq holiday-christian-holidays nil) ; get rid of christan holidays
  (setq calendar-view-holidays-initially-flag t))

(use-package cal-china
  :config
  ;; display the ‘celestial-stem’ (天干) and the ‘terrestrial-branch’ (地支) in Chinese:
  (setq calendar-chinese-celestial-stem
        ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"]
        calendar-chinese-terrestrial-branch
        ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"])
  )

;; [ cal-china-x ] -- Chinese localization, lunar/horoscope/zodiac info and more...
(use-package cal-china-x
  :ensure t
  :config
  (setq calendar-mark-holidays-flag t)
  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
  (setq cal-china-x-general-holidays '((holiday-lunar 12 23 "小年")
                                       (holiday-lunar 1 15 "元宵节")
                                       (holiday-lunar 9 9 "重阳节")))
  (setq calendar-holidays
        (append cal-china-x-important-holidays
                cal-china-x-general-holidays
                holiday-general-holidays
                holiday-local-holidays
                holiday-other-holidays))
  )


;; [ Diary ] -- Diary markers in Calendar.

;;; [ calfw ] -- Calendar framework for Emacs

(use-package calfw
  :ensure t
  :defer t
  :bind (:map calendar-prefix
              ("o" . cfw:open-org-calendar)
              ("x" . cfw:open-calendar-buffer))
  :config
  ;; Faces
  ;; Year / Month
  (set-face-attribute 'cfw:face-title nil
                      :foreground "forest green"
                      :weight 'bold
                      :height 2.3)
  ;; 1-31
  (set-face-attribute 'cfw:face-default-day nil
                      :foreground "white"
                      )
  ;; 1-31 (N)
  (set-face-attribute 'cfw:face-day-title nil
                      :foreground "dim gray")
  ;; Week (1-5)
  (set-face-attribute 'cfw:face-header nil
                      :foreground "sky blue"
                      :weight 'bold)
  ;; Saturday
  (set-face-attribute 'cfw:face-saturday nil
                      :inherit 'cfw:face-day-title
                      :foreground "deep pink"
                      :weight 'bold
                      ;; :box '(:color "orange" :line-width -1)
                      )
  ;; Sunday
  (set-face-attribute 'cfw:face-sunday nil
                      :inherit 'cfw:face-day-title
                      :foreground "deep pink"
                      :weight 'bold
                      ;; :box '(:color "yellow" :line-width -1)
                      )
  ;; Holidays
  (set-face-attribute 'cfw:face-holiday nil
                      :inherit 'cfw:face-day-title
                      :foreground "yellow"
                      ;; :weight 'normal
                      :overline t
                      ;; :box '(:color "yellow" :line-width -1)
                      )
  ;; Grid
  (set-face-attribute 'cfw:face-grid nil
                      :foreground "#333333"
                      )
  ;; ??
  (set-face-attribute 'cfw:face-default-content nil
                      :foreground "gray")
  ;; ??
  (set-face-attribute 'cfw:face-periods nil
                      :foreground "cyan")
  ;; ??
  (set-face-attribute 'cfw:face-annotation nil
                      :foreground "dark green"
                      )
  ;; past days
  (set-face-attribute 'cfw:face-disable nil
                      :foreground "#333333"
                      :strike-through t)
  ;; today (N)
  (set-face-attribute 'cfw:face-today-title nil
                      :inherit 'cfw:face-day-title
                      :foreground "cyan"
                      :weight 'bold
                      ;; :box '(:color "dark cyan" :line-width -1)
                      )
  ;; today events grid
  (set-face-attribute 'cfw:face-today nil
                      :inherit 'cfw:face-day-title
                      :foreground "dark gray"
                      :weight 'normal
                      )
  ;; current select
  (set-face-attribute 'cfw:face-select nil
                      :foreground "white"
                      :background "deep pink"
                      )
  ;; toolbar [ < ] [ > ] [Today ]                      [Day] [Week] [Two Weeks] [Month]
  (set-face-attribute 'cfw:face-toolbar nil
                      :foreground "gray")
  (set-face-attribute 'cfw:face-toolbar-button-on nil
                      :foreground "white"
                      :weight 'bold
                      :box '(:color "yellow" :line-width 1))
  (set-face-attribute 'cfw:face-toolbar-button-off nil
                      :foreground "dim gray"
                      :weight 'normal)

  ;; Grid frame
  ;; Another unicode chars
  (setq cfw:fchar-junction ?╬
        cfw:fchar-vertical-line ?║
        cfw:fchar-horizontal-line ?═
        cfw:fchar-left-junction ?╠
        cfw:fchar-right-junction ?╣
        cfw:fchar-top-junction ?╦
        cfw:fchar-top-left-corner ?╔
        cfw:fchar-top-right-corner ?╗)

  ;; Line breaking
  (setq cfw:render-line-breaker 'cfw:render-line-breaker-wordwrap)


  ;; for Org-mode
  (require 'calfw-org)

  ;; (setq cfw:org-agenda-schedule-args '(:timestamp))
  ;; (setq cfw:org-overwrite-default-keybinding nil)

  ;; org-capture template
  (setq cfw:org-capture-template
        '("c" "calfw2org" entry
          (file nil)
          "* %?\n %(cfw:org-capture-day)"))

  ;; for iCalendar (Google Calendar) users:
  ;; (require 'calfw-ical)
  ;; (cfw:open-ical-calendar "http://www.google.com/calendar/ical/.../basic.ics")

  ;; for Diary users:
  ;; - [M-x cfw:open-diary-calendar]
  ;; (require 'calfw-cal)

  ;; calfw-gcal.el -- edit Google Calendar for calfw.
  ;; (require 'calfw-gcal)

  ;; for Howm users:
  ;; (require 'calfw-howm)


  ;; General setting
  (defun my-open-calfw-week ()
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources
     (list
      (cfw:org-create-source "dark gray") ; Org-mode source
      ;; (cfw:cal-create-source "orange") ; Diary source
      ;; (cfw:ical-create-source "Moon" "~/moon.ics" "Gray") ; iCalendar source1
      ;; (cfw:ical-create-source "gcal" "https://..../basic.ics" "IndianRed") ; Google Calendar ICS
      ;; (cfw:howm-create-source "blue") ; howm source
      )
     ;; :annotation-sources
     ;; (list
     ;;  (cfw:ical-create-source "Moon" "~/moon.ics" "Gray") ; Moon annotations
     ;;  )
     :view 'week                         ; 'month, 'week, 'day
     )
    (bury-buffer)
    (switch-to-buffer "*cfw-calendar*")
    ;; (toggle-frame-maximized)
    ;; (cfw:refresh-calendar-buffer t)
    )

  (defun my-open-calfw-day ()
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources
     (list
      (cfw:org-create-source "dark gray") ; Org-mode source
      (cfw:cal-create-source "orange") ; Diary source
      )
     :view 'day                           ; 'month, 'week, 'day
     )
    (bury-buffer)
    (switch-to-buffer "*cfw-calendar*")
    )

  (defun my-open-calfw-month ()
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources
     (list
      (cfw:org-create-source "dark gray") ; Org-mode source
      (cfw:cal-create-source "orange") ; Diary source
      )
     :view 'month                         ; 'month, 'week, 'day
     )
    (bury-buffer)
    (switch-to-buffer "*cfw-calendar*")
    )

  (define-key calendar-prefix (kbd "c") 'my-open-calfw-week)
  (define-key calendar-prefix (kbd "w") 'my-open-calfw-week)
  (define-key calendar-prefix (kbd "d") 'my-open-calfw-day)
  (define-key calendar-prefix (kbd "m") 'my-open-calfw-month)

  ;; for org-agenda
  (use-package calfw-org
    :ensure t)
  ;; for Emacs Diary
  (use-package calfw-cal
    :ensure t)
  ;; for iCal format
  (use-package calfw-ical
    :ensure t)
  )


;;; [ iCalendar ]

;; FIXME: deprecated variable (free assigned).
;; (setq org-combined-agenda-icalendar-file "~/Org/Calendar/iCalendar.ics")

;;; [ Google Calendar ]

;;;  [ org-gcal ]

;; (use-package org-gcal
;;   :ensure t
;;   :config
;;   (setq org-gcal-client-id "1019475480650-hrce54ct4su6stomeq4n0ka2jn8sh4i1.apps.googleusercontent.com"
;;         org-gcal-client-secret "iBXd-WZYFlUKAFJwlKdviaoT"
;;         org-gcal-file-alist '(("numbchild@gmail.com" .  "~/Org/Calendars/Google Calendar.org")
;;                               ;; ("another-mail@gmail.com" .  "~/task.org")
;;                               ))
;;
;;   (add-hook 'org-agenda-mode-hook #'org-gcal-sync)
;;   (add-hook 'org-capture-after-finalize-hook #'org-gcal-sync)
;;   )



(provide 'init-my-tool-calendar)

;;; init-my-tool-calendar.el ends here
