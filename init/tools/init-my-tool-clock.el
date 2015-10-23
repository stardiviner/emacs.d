;;; init-my-tool-clock.el --- init Clock tools for Emacs
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

(if (featurep 'helm)
    ;; `helm-source-time-world'
    (progn
      ;; the time zone of your home
      (setq helm-time-zone-home-location "Shanghai")
      (define-key my-tools-prefix (kbd "t") 'helm-world-time)
      )
  (define-key my-tools-prefix (kbd "t") 'display-time-world))


(provide 'init-my-tool-clock)

;;; init-my-tool-clock.el ends here
