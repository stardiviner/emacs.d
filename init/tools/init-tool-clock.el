;;; init-tool-clock.el --- init Clock tools for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

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

(add-to-list 'display-buffer-alist
             '("\\*wclock\\*" . (display-buffer-reuse-window display-buffer-below-selected)))


(provide 'init-tool-clock)

;;; init-tool-clock.el ends here
