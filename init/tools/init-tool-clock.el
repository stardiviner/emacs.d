;;; init-tool-clock.el --- init Clock tools for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ timeclock ] -- mode for keeping track of how much you work similar with `org-clock-in' and `org-clock-out'.

;;; [ World Clock ]

;;; Emacs default `display-time-mode' has this world clock list support.

(setq display-time-world-list '(("Asia/Shanghai" "Shanghai")
                                ("Asia/Tokyo" "Tokyo")
                                ("America/Los_Angeles" "Silicon Valley")
                                ("America/New_York" "New York")
                                ("America/Los_Angeles" "Los Angeles")
                                ("Europe/Paris" "Paris")
                                ("Europe/London" "London")
                                ))

(define-key tools-prefix (kbd "t") 'display-time-world)

(add-to-list 'display-buffer-alist '("\\*wclock\\*" . (display-buffer-below-selected)))

;;; [ alarm-clock ] -- An alarm clock for Emacs.

;; (use-package alarm-clock
;;   :ensure t
;;   :commands (alarm-clock-set alarm-clock-list-view))

;;; [ egg-timer ] -- Commonly used intervals for setting timers while working.

;; (use-package egg-timer
;;   :ensure t
;;   :commands (egg-timer-schedule))


(provide 'init-tool-clock)

;;; init-tool-clock.el ends here
