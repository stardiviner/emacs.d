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
(if (file-exists-p org-clock-persist-file)
    ;; (setq org-clock-persist 'history)
    (org-clock-persistence-insinuate)
  (shell-command (concat "touch " org-clock-persist-file)))

;; (add-hook 'org-clock-out-hook 'org-clock-remove-empty-clock-drawer) ; `org-clock-out-remove-zero-time-clocks'

(define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
(define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu)

;;; Sounds:
;; "~/.emacs.d/resources/audio/Ingress/Speech/speech_hacking.wav"
;; "~/.emacs.d/resources/audio/Ingress/SFX/sfx_typing.wav"
;; "~/.emacs.d/resources/audio/Hacking Game/voice-complete.wav"
;; "~/.emacs.d/resources/audio/Hacking Game/voice-loading.wav"
;; "~/.emacs.d/resources/audio/Hacking Game/hesfx_untold_tick2.wav"

(add-hook 'org-clock-in-hook
          (lambda ()
            (org-clock-play-sound
             "~/.emacs.d/resources/audio/Ingress/Speech/speech_hacking.wav")))
(add-hook 'org-clock-in-prepare-hook
          (lambda ()
            (org-clock-play-sound
             "~/.emacs.d/resources/audio/Ingress/SFX/sfx_typing.wav")))
(add-hook 'org-clock-out-hook
          (lambda ()
            (org-clock-play-sound
             "~/.emacs.d/resources/audio/Hacking Game/voice-complete.wav")))
(add-hook 'org-clock-cancel-hook
          (lambda ()
            (org-clock-play-sound
             "~/.emacs.d/resources/audio/Hacking Game/hesfx_untold_tick2.wav")))


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


;;; [ Effort Estimates ]

;; to add an effort estimate "on the fly".
(add-hook 'org-clock-in-prepare-hook 'org-clock-modify-effort-estimate)
;; (remove-hook 'org-clock-in-hook 'org-clock-modify-effort-estimate)

;; setup column views for effort estimates
(setq org-columns-default-format "%25ITEM %TODO %3PRIORITY %TAGS %6effort(EFFORT){:}"
      ;; org-global-properties
      org-agenda-columns-add-appointments-to-effort-sum t
      org-agenda-columns-compute-summary-properties t
      )


;;; [ org-time-budgets ]

(use-package org-time-budgets
  ;; :ensure t
  :config
  (setq org-time-budgets
        '((:title "My Learning Plan" :tag "+learn" :budget "35:00" :block 'week)
          ;; (:title "" :tags "+play" :budget "30:00" :block 'weekend)

          (:title "Emacs" :tag "+learn" :budget "21:00" :block 'week)
          (:title "Ruby on Rails" :tag "+learn" :budget "21:00" :block 'week)
          (:title "Ruby" :tag "+learn" :budget "21:00" :block 'week)
          (:title "Web" :tag "+learn" :budget "21:00" :block 'week)
          (:title "Julia" :tag "+learn" :budget "15:00" :block 'week)
          (:title "Lisp" :tag "+learn" :budget "10:00" :block 'week)
          (:title "Clojure" :tag "+learn" :budget "14:00" :block 'week)
          ))


  ;; adding `org-time-budgets' to your Agenda.
  (add-to-list 'org-agenda-custom-commands
               '(("a" "Agenda"
                  ((agenda ""
                           ((org-agenda-sorting-strategy
                             '(habit-down time-up priority-down category-keep user-defined-up))))
                   (org-time-budgets-for-agenda)))))
  )



(provide 'init-my-org-time)

;;; init-my-org-time.el ends here
