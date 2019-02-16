;;; init-tool-calendar.el --- init for Calendar
;;; -*- coding: utf-8 -*-

;;; Commentary:


;;; Code:

(unless (boundp 'calendar-prefix)
  (define-prefix-command 'calendar-prefix))
(define-key tools-prefix (kbd "c") 'calendar-prefix)

;;; [ calendar ]

(use-package calendar
  :ensure t
  :defer t
  :bind (:map calendar-prefix ("c" . calendar))
  :init
  ;; set calendar style
  (setq calendar-week-start-day 1 ; 0:Sunday, 1:Monday
        calendar-weekend-days '(0 6)
        ;; 'american: month/day/year, 'european: day/month/year, 'iso: year/month/day
        calendar-date-style 'iso)

  ;; Annotations
  ;; variable -> :annotation-sources
  
  ;; Calendar Location: Latitude, Longitude.
  ;;
  ;; for predicate lunar eclipses.
  ;;
  ;; - Zhejiang, China Area: Latitude: 27° 09' ~ 31° 11' N , Longitude: 118° 02' ~ 122° 57' E
  ;; - Shaoxing Area: Latitude: 29° 42' ~ 30° 19' 15" , Longitude: 120° 16' 55" ~ 120° 46' 39"
  ;; - XiaJiangChun, Zhuji, Zhejiang, China: 29.90256956936341, 120.37954302845002
  ;; - Yunnan, Dali: Latitude: 25.60, Longitude: 100.23,
  ;; - Zhejiang, Shaoxing, Zhuji: 29.72, 120.20, UTC+8 (China): +480
  (setq calendar-location-name "Shaoxing Town"
        calendar-time-zone +480
        calendar-latitude 29.90256956936341
        calendar-longitude 120.37954302845002)

  ;; mark holidays
  (setq calendar-mark-holidays-flag t
        calendar-view-holidays-initially-flag t)
  ;; mark today
  (add-hook 'calendar-initial-window-hook 'calendar-mark-today)
  ;; mark diary entries
  ;; (setq calendar-mark-diary-entries-flag t)
  )

;;; Localized National Holidays
(use-package holidays
  :init
  ;; `calfw' collects holidays from function `calendar-holiday-list' and the
  ;; customize variable `calendar-holidays' which belongs to `holidays.el` in
  ;; Emacs.
  ;; get rid of U.S. holidays
  ;; (setq holiday-general-holidays nil)
  ;; get rid of christan holidays
  (setq holiday-christian-holidays nil))

(use-package cal-china
  :init
  ;; display the ‘celestial-stem’ (天干) and the ‘terrestrial-branch’ (地支) in Chinese:
  (setq calendar-chinese-celestial-stem
        ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"]
        calendar-chinese-terrestrial-branch
        ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"]))

;; [ cal-china-x ] -- Chinese localization, lunar/horoscope/zodiac info and more...

(use-package cal-china-x
  :ensure t
  :config
  (setq calendar-mark-holidays-flag t)
  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
  (setq cal-china-x-general-holidays '((holiday-lunar 1 1 "春节")
                                       (holiday-lunar 1 14 "祭灶")
                                       (holiday-lunar 1 15 "元宵节(上元节)")
                                       (holiday-lunar 2 2 "龙抬头(土地诞)")
                                       (holiday-lunar 3 3 "上巳节")
                                       (holiday-lunar 7 7 "七夕节")
                                       (holiday-lunar 7 14 "七月半(中元节)")
                                       (holiday-lunar 7 15 "七月半(中元节)")
                                       (holiday-lunar 8 15 "中秋节")
                                       (holiday-lunar 9 9 "重阳节")
                                       (holiday-fixed 10 1 "寒衣节")
                                       (holiday-fixed 10 15 "下元节")
                                       (holiday-lunar 12 8 "腊八节")
                                       (holiday-lunar 12 23 "小年")
                                       (holiday-lunar 12 24 "小年")
                                       (holiday-lunar 12 29 "除夕(岁除)")
                                       (holiday-lunar 12 30 "除夕(岁除)")))
  (setq cal-china-x-solar-days '((holiday-fixed 4 3 "寒食节")
                                 (holiday-fixed 4 4 "寒食节")
                                 (holiday-fixed 4 5 "清明节")
                                 (holiday-solar-term "冬至" "冬至节")
                                 (holiday-fixed 12 21 "冬至节")
                                 (holiday-fixed 12 22 "冬至节")
                                 (holiday-fixed 12 23 "冬至节")))
  (setq cal-china-x-ethical-holidays '((holiday-fixed 4 15 "傣族 - 泼水节")
                                       (holiday-fixed 7 10 "蒙古族 - 那达慕大会")
                                       (holiday-lunar 6 24 "彝族 - 火把节")
                                       (holiday-lunar 5 29 "瑶族 - 达努节")
                                       (holiday-lunar 3 15 "白族 - 三月街")
                                       (holiday-lunar 3 16 "白族 - 三月街")
                                       (holiday-lunar 3 17 "白族 - 三月街")
                                       (holiday-lunar 3 18 "白族 - 三月街")
                                       (holiday-lunar 3 19 "白族 - 三月街")
                                       (holiday-lunar 3 20 "白族 - 三月街")
                                       (holiday-lunar 3 21 "白族 - 三月街")
                                       ;; ( "壮族 - 歌圩")
                                       ;; ( "藏族 - 藏历年")
                                       ;; ( "藏族 - 望果节")
                                       ;; ( "苗族 - 跳花节")
                                       ))
  ;; [M-x holidays] from function `holiday-list'.
  (setq calendar-holidays
        (append cal-china-x-important-holidays
                cal-china-x-general-holidays
                ;; cal-china-x-japanese-holidays
                cal-china-x-solar-days
                cal-china-x-ethical-holidays
                holiday-general-holidays
                holiday-local-holidays
                holiday-other-holidays
                holiday-solar-holidays
                holiday-oriental-holidays)))

;;; [ calfw ] -- Calendar framework for Emacs

(use-package calfw
  :ensure t
  :bind (:map calendar-prefix ("x" . cfw:open-calendar-buffer))
  :init
  ;; Grid frame
  (setq cfw:fchar-junction ?╬
        cfw:fchar-vertical-line ?║
        cfw:fchar-horizontal-line ?═
        cfw:fchar-left-junction ?╠
        cfw:fchar-right-junction ?╣
        cfw:fchar-top-junction ?╦
        cfw:fchar-top-left-corner ?╔
        cfw:fchar-top-right-corner ?╗)
  :config
  ;; General setting
  ;; (require 'calfw-org)
  (defun calfw:week ()
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources (list
                        ;; (cfw:org-create-source "dark gray") ; Org-mode source
                        ;; (cfw:cal-create-source "orange") ; Diary source
                        ;; (cfw:ical-create-source "Moon" "~/moon.ics" "Gray") ; iCalendar source1
                        ;; (cfw:ical-create-source "gcal" "https://..../basic.ics" "IndianRed") ; Google Calendar ICS
                        ;; (cfw:howm-create-source "blue") ; howm source
                        )
     ;; :annotation-sources (list (cfw:ical-create-source "Moon" "~/moon.ics" "Gray"))
     :view 'week)
    (bury-buffer)
    (switch-to-buffer "*cfw-calendar*")
    ;; (toggle-frame-maximized)
    ;; (cfw:refresh-calendar-buffer t)
    )

  (defun calfw:day ()
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources
     (list )
     :view 'day)
    (bury-buffer)
    (switch-to-buffer "*cfw-calendar*"))

  (defun calfw:month ()
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources (list 
                        ;; (cfw:org-create-source "dark gray")
                        )
     :view 'month)
    (bury-buffer)
    (switch-to-buffer "*cfw-calendar*"))

  (define-key calendar-prefix (kbd "w") 'calfw:week)
  (define-key calendar-prefix (kbd "d") 'calfw:day)
  (define-key calendar-prefix (kbd "m") 'calfw:month))

;;; [ calfw-org ] -- calendar view for org-agenda.

(use-package calfw-org
  :ensure t
  :commands (cfw:open-org-calendar)
  :bind (:map calendar-prefix ("o" . cfw:open-org-calendar))
  :init (setq cfw:org-capture-template
              '("D" "[D] calfw2org" entry
                (file nil)
                "* %?\n %(cfw:org-capture-day)")))

;; [ calfw-cal ] -- for Emacs Diary

;; (use-package calfw-cal
;;   :ensure t)

;;; [ iCalendar ] -- for for iCalendar (Google Calendar) users

;; (use-package calfw-ical
;;   :ensure t
;;   :config
;;   ;; (cfw:open-ical-calendar "http://www.google.com/calendar/ical/.../basic.ics")
;;   )

;; FIXME: deprecated variable (free assigned).
;; (setq org-combined-agenda-icalendar-file "~/Org/Calendar/iCalendar.ics")

;;; [ calfw-gcal ] -- edit Google Calendar for calfw.

;; (use-package calfw-gcal
;;   :ensure t)

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

;; for Howm users:
;; (use-package calfw-howm
;;   :ensure t)



(provide 'init-tool-calendar)

;;; init-tool-calendar.el ends here
