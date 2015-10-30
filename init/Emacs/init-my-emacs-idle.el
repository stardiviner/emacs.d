;;; init-my-emacs-idle.el --- init for when Emacs Idle
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Zone out ]

;;; Usage:
;;
;; - [M-x zone] :: Zone out, completely.
;; - [M-x zone-mode] :: screensaver.
;; - [M-x zone-when-idle] :: start Zone out when idle.
;; - press any key, like [C-n] to quit Zone.
;; - [M-x zone-leave-me-alone] :: quit Zone.

(autoload 'zone-when-idle "zone.el" nil t)
(zone-when-idle (* 60 5))


;;; [ zone-nyan ]

;;; Usage:
;;
;; - [M-x zone-nyan-preview] :: preview zone-nyan.

(use-package zone-nyan
  :config
  (setq zone-programs [zone-nyan])
  )


(provide 'init-my-emacs-idle)

;;; init-my-emacs-idle.el ends here
