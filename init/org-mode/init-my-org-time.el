;;; init-my-org-time.el --- init for Org Time & Date
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Time Stamps ]

(defun my-org-headline-add-date-property ()
  "Auto add/update/append DATE property for new created headline."
  (interactive)
  (when (yes-or-no-p "Add DATE property with value of inactive time-stamp? ")
    (unless (assoc "DATE" (org-entry-properties nil nil))
      (org-set-property
       "DATE"
       (format "%s" (with-temp-buffer
                      (org-time-stamp 4 'inactive)
                      (buffer-string)))))
    ;; remove the inserted time-stamp in headline.
    (org-back-to-heading t)
    (org-beginning-of-line)
    (org-kill-line)))

(define-key org-mode-map (kbd "C-c C-x M-t") 'my-org-headline-add-date-property)
;; (add-hook 'org-insert-heading-hook #'my-org-headline-add-date-property)

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


(provide 'init-my-org-time)

;;; init-my-org-time.el ends here
