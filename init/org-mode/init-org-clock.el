;;; init-org-clock.el --- init for Org-mode timer and clock.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Timer ]

(use-package org-timer
  :defer t
  ;; Pomodoro time management technique.
  :init (setq org-timer-default-timer 25))

;;; [ Clock ]

(use-package org-clock
  :defer t
  :init
  ;;; Persistent clock
  (setq org-clock-persist t ; nil, t, 'clock, 'history
        org-clock-persist-query-save t
        org-clock-persist-query-resume nil ; don't ask, resume by default when Emacs restart.
        org-clock-persist-file (expand-file-name "org-clock-save.el" user-emacs-directory))
  :config
  ;;; To save the clock history across Emacs sessions, use
  (if (file-exists-p org-clock-persist-file)
      ;; (setq org-clock-persist 'history)
      (org-clock-persistence-insinuate)
    (shell-command (concat "touch " org-clock-persist-file)))

  (setq org-clock-in-resume t    ; resume when clock in.
        org-clock-sound (expand-file-name
                         "resources/audio/Ingress/Speech/speech_hacking.wav" user-emacs-directory)
        org-clock-mode-line-total 'current
        ;; org-agenda-start-with-clockreport-mode t
        org-clock-idle-time nil ; 5
        org-clock-history-length 100)

  (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
  (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu)

  (add-hook 'org-clock-in-prepare-hook #'sound-tick)
  ;; (add-hook 'org-clock-in-hook #'sound-voice-hacking)
  (add-hook 'org-clock-out-hook #'sound-success)
  (add-hook 'org-clock-cancel-hook #'sound-newmessage)

  ;; auto clocking out after Emacs idle time.
  ;; (org-clock-auto-clockout-insinuate)
  ;; (setq org-clock-auto-clockout-timer (* 60 2))
  )

;;; [ counsel-org-clock ] -- Counsel (Ivy) interface for org-clock.

(use-package counsel-org-clock
  :ensure t
  :defer t
  :commands (counsel-org-clock-history counsel-org-clock-context)
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
