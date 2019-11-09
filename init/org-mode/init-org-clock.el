;;; init-org-clock.el --- init for Org-mode timer and clock.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Timer ]

(require 'org-timer)

(setq org-timer-default-timer 25)       ; Pomodoro time management technique.
(setq org-timer-display 'mode-line)


;;; [ Clock ]

(setq org-clock-persist t ; nil, t, 'clock, 'history
      org-clock-persist-query-save t
      org-clock-persist-query-resume nil ; don't ask, resume by default when Emacs restart.
      org-clock-persist-file (expand-file-name "org-clock-save.el" user-emacs-directory)
      org-clock-in-resume t    ; resume when clock in.
      org-clock-continuously nil ; don't continue on last clock out.
      org-clock-in-switch-to-state nil ; "STARTED"
      org-clock-out-when-done t         ; clock will stop when task marked DONE.
      org-clock-into-drawer t  ; Save clock data and notes in the :LOGBOOK: drawer
      org-clock-out-remove-zero-time-clocks t ; Removes clocked tasks with 0:00 duration
      org-clock-sound (expand-file-name
                       "resources/audio/Ingress/Speech/speech_hacking.wav" user-emacs-directory)
      ;; 'mode-line, 'frame-title, 'both, nil.
      ;; org-clock-clocked-in-display 'both ; 'frame-title will make window frame border resize.
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
      org-clock-history-length 20 ; keep a long clocking tasks history for easy re-select history task to clock.
      )

;;; `org-clock-display'
;; (setq org-clock-display-default-range 'thisyear) 'untilnow,


;;; To save the clock history across Emacs sessions, use
(if (file-exists-p org-clock-persist-file)
    ;; (setq org-clock-persist 'history)
    (org-clock-persistence-insinuate)
  (shell-command (concat "touch " org-clock-persist-file)))

;; (add-hook 'org-clock-out-hook 'org-clock-remove-empty-clock-drawer) ; `org-clock-out-remove-zero-time-clocks'

(define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
(define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu)

(add-hook 'org-clock-in-prepare-hook #'sound-tick)
;; (add-hook 'org-clock-in-hook #'sound-voice-hacking)
(add-hook 'org-clock-out-hook #'sound-success)
(add-hook 'org-clock-cancel-hook #'sound-newmessage)

;;; [ counsel-org-clock ] -- Counsel (Ivy) interface for org-clock.

(use-package counsel-org-clock
  :ensure t
  :defer t
  :bind (:map org-clock-prefix
              ("C-i" . counsel-org-clock-history)
              ("C-c" . counsel-org-clock-context)))

;;; display org-clock in head-line.
;; (setq org-clock-clocked-in-display nil)
;; (defun my/show-org-clock-in-header-line ()
;;   (setq-default header-line-format '((" " org-mode-line-string " "))))
;; (defun my/remove-org-clock-in-header-line ()
;;   (setq-default header-line-format nil))
;; (add-hook 'org-clock-in-hook #'my/show-org-clock-in-header-line)
;; (add-hook 'org-clock-out-hook #'my/remove-org-clock-in-header-line)
;; (add-hook 'org-clock-cancel-hook #'my/remove-org-clock-in-header-line)

;;; [ org-analyzer ] -- a tool that extracts time tracking data from org files.

(use-package org-analyzer
  :ensure t
  :defer t
  :commands (org-analyzer-start))


(provide 'init-org-clock)

;;; init-org-clock.el ends here
