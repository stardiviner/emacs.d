;;; init-my-org-time.el --- init for Org Time & Date
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Time Stamps ]


;;; [ Time Interval ]

(defun org-time-interval (&optional arg)
  "Set schedule and deadline time interval for headline.

Accepts universal argument \\<C-c C-x r> & \\[org-time-interval]."
  (interactive "P")
  ;; C-u is '(4) and C-u C-u is '(16)
  ;; (equal arg '(4))
  ;; So I need to use `(interactive "p")' for `(org-deadline)'.
  (org-schedule arg)
  ;; (org-deadline arg "+3d") ; this is not interactive for deadline.
  (org-deadline arg))

(define-key org-mode-map (kbd "C-c C-x r") 'org-time-interval)


;;; [ Effort Estimates ] -- [C-c C-x C-c] + [C-c C-x C-e]

;; to add an effort estimate "on the fly".
(add-hook 'org-clock-in-prepare-hook 'org-clock-modify-effort-estimate)
;; (remove-hook 'org-clock-in-hook 'org-clock-modify-effort-estimate)

;; setup column views for effort estimates
(setq org-columns-default-format "%25ITEM %TODO %3PRIORITY %TAGS %8Effort(Effort){:}"
      ;; org-global-properties
      org-agenda-columns-add-appointments-to-effort-sum t
      org-agenda-columns-compute-summary-properties t
      )


;;; [ org-pomodoro ] -- adds support for Pomodoro technique in Org-mode.

(use-package org-pomodoro
  :ensure t
  :commands (org-pomodoro)
  :init
  (define-key org-agenda-mode-map (kbd "p") 'org-pomodoro)
  (define-key my-org-prefix (kbd "p") 'org-pomodoro)
  :config
  (setq org-pomodoro-audio-player "/usr/bin/mplayer"
        org-pomodoro-play-sounds t
        org-pomodoro-play-start-sound t
        org-pomodoro-play-ticking-sounds nil
        ;; org-pomodoro-ticking-sound
        org-pomodoro-ticking-sound-args "-volume 50" ; adjust ticking sound volume
        ;; org-pomodoro-start-sound-args "-volume 0.3"
        ;; org-pomodoro-long-break-sound-args "-volume 0.3"
        org-pomodoro-format "Pomodoro~%s" ; mode-line string
        )

  ;; start another pomodoro automatically upon a break end.
  (add-hook 'org-pomodoro-break-finished-hook
            (lambda ()
              (interactive)
              (org-pomodoro '(16)) ; double prefix [C-u C-u]
              ))

  (defun my/org-pomodoro-text-time ()
    "Display remaining pomodoro time in i3 status bar.

Usage:
emacsclient -e '(my/org-pomodoro-text-time)'"
    (if (org-pomodoro-active-p)
        (format "Pomodoro: %d minutes - %s"
                (/ org-pomodoro-countdown 60) org-clock-heading)
      "No active pomodoro"))
  )

;;; [ tomatinho ] -- A simple and beautiful pomodoro technique timer that runs on Emacs.

(use-package tomatinho
  :ensure t
  :bind ("<f12>" . tomatinho)
  )



(provide 'init-my-org-time)

;;; init-my-org-time.el ends here
