;;; init-org-time.el --- init for Org Time & Date
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Time Stamps ]

(defun my/org-property-add-date-auto ()
  "A helper command for adding DATE/TIME property and value automatically."
  (interactive)
  (save-excursion
    (org-set-property "DATE" (format-time-string "[%Y-%m-%d %a %H:%M]"))))
(define-key org-mode-map (kbd "C-c C-x M-t") 'my/org-property-add-date-auto)
;; (add-hook 'org-insert-heading-hook #'my/org-property-add-date-auto)

;;; [ Time Interval ]

(defun org-time-interval (arg)
  "Set schedule and deadline time interval for headline.

Accepts universal argument `ARG' for \\<C-c C-x r> & \\[org-time-interval]."
  (interactive "P")
  ;; C-u is '(4) and C-u C-u is '(16)
  ;; (equal arg '(4))
  ;; So I need to use `(interactive "p")' for `(org-deadline)'.
  (org-schedule arg)
  ;; (org-deadline arg "+3d") ; this is not interactive for deadline.
  (org-deadline arg))

(define-key org-mode-map (kbd "C-c C-x M-r") 'org-time-interval)


;;; [ Effort Estimates ] -- [C-c C-x C-e]

;; to add an effort estimate "on the fly".
(add-hook 'org-clock-in-prepare-hook 'org-clock-modify-effort-estimate)
;; (add-hook 'org-clock-in-hook 'org-clock-modify-effort-estimate)



;;; [ org-pomodoro ] -- adds support for Pomodoro technique in Org-mode.

(use-package org-pomodoro
  :ensure t
  :defer t
  :commands (org-pomodoro)
  :bind (:map Org-prefix ("p" . org-pomodoro))
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
            #'(lambda ()
                (interactive)
                (org-pomodoro '(16)) ; double prefix [C-u C-u]
                ))
  )


(provide 'init-org-time)

;;; init-org-time.el ends here
