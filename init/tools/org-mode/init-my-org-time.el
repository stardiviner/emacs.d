;;; init-my-org-time.el --- init for Org Time & Date
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


;;; [ Time Stamps ]



;;; [ Timer ]
(eval-after-load "org"
  '(progn
     (require 'org-timer)
     (add-to-list 'org-modules 'org-timer)
     ))

(setq org-timer-default-timer 25)       ; Pomodoro time management technique.
(setq org-timer-display 'mode-line)


;;; [ Clock ]
(eval-after-load "org"
  '(progn
     (require 'org-clock)))

(setq org-clock-persist t ; nil, t, 'clock, 'history
      org-clock-persist-query-save t
      org-clock-persist-query-resume t
      org-clock-persist-file "~/.emacs.d/org-clock-save.el"
      org-clock-in-resume t    ; resume when clock in.
      org-clock-continuously nil ; don't continue on last clock out.
      org-clock-in-switch-to-state "STARTED"
      org-clock-out-when-done t         ; clock will stop when task marked DONE.
      org-clock-into-drawer t  ; Save clock data and notes in the :LOGBOOK: drawer
      org-clock-out-remove-zero-time-clocks t ; Removes clocked tasks with 0:00 duration
      org-clock-sound "~/.emacs.d/resources/audio/Ingress/Speech/speech_hacking.wav"
      ;; 'mode-line, 'frame-title, 'both, nil.
      org-clock-clocked-in-display 'frame-title
      ;; org-clock-mode-line-entry t
      org-clock-mode-line-total 'auto
      ;; org-clock-clocktable-language-setup
      ;; org-clock-leftover-time
      ;; org-clock-task-overrun
      ;; org-clock-task-overrun-text
      ;; org-clock-clocktable-default-properties '(:maxlevel 2 :scope file)
      org-clock-report-include-clocking-task t
      ;; org-agenda-clockreport-mode
      ;; org-agenda-start-with-clockreport-mode t
      org-clock-goto-may-find-recent-task t
      ;; org-clock-total-time-cell-format "*%s*"
      org-clock-idle-time nil             ; t
      ;; org-clock-auto-clock-resolution 'when-no-clock-is-running
      ;; org-clock-resolve-expert t
      )

;;; To save the clock history across Emacs sessions, use
;; (setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; to add an effort estimate "on the fly".
(add-hook 'org-clock-in-prepare-hook 'org-clock-modify-effort-estimate)
;; (add-hook 'org-clock-out-hook 'org-clock-remove-empty-clock-drawer) ; `org-clock-out-remove-zero-time-clocks'

(define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
(define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu)

;; Modify the org-clock-in so that a timer is started with the default value
;; except if a timer is already started:
;;
;; (add-hook 'org-clock-in-hook
;;           '(lambda ()
;;              (if (not org-timer-current-timer) ; FIXME: this variable seems not part of `org-timer'.
;;                  (org-timer-set-timer '(16)))))

(if (featurep 'helm)
    (progn
      (require 'helm-org)
      (advice-add 'org-clock-select-task :override #'helm-org-clock-select-task)
      ;; (advice-remove 'org-clock-select-task #'helm-org-clock-select-task)
      ))
;; fix org clock in does not play sound.
(add-hook 'org-clock-in-hook 'org-clock-play-sound)


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




(provide 'init-my-org-time)

;;; init-my-org-time.el ends here
