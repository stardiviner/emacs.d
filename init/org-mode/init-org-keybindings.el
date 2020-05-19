;;; init-org-keybindings.el --- init for Org Keybindings
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Keybindings ]

(define-key Org-prefix (kbd "a") 'org-agenda)
(define-key Org-prefix (kbd "c") 'org-capture)

(unless (boundp 'org-link-prefix)
  (define-prefix-command 'org-link-prefix))
(define-key Org-prefix (kbd "C-l") 'org-link-prefix)
(define-key org-link-prefix (kbd "l") 'org-store-link)
(define-key org-link-prefix (kbd "C-l") 'org-insert-link-global)
(define-key org-link-prefix (kbd "C-o") 'org-open-at-point-global)

(unless (boundp 'org-timer-prefix)
  (define-prefix-command 'org-timer-prefix))
(define-key Org-prefix (kbd "C-t") 'org-timer-prefix)
(define-key org-timer-prefix (kbd ".") 'org-timer)
(define-key org-timer-prefix (kbd ";") 'org-timer-set-timer)
(define-key org-timer-prefix (kbd "0") 'org-timer-start)
(define-key org-timer-prefix (kbd "_") 'org-timer-stop)
(define-key org-timer-prefix (kbd ",") 'org-timer-pause-or-continue)
(define-key org-timer-prefix (kbd "-") 'org-timer-item)
(define-key org-timer-prefix (kbd "'") 'org-timer-show-remaining-time)

(unless (boundp 'org-clock-prefix)
  (define-prefix-command 'org-clock-prefix))
(define-key Org-prefix (kbd "C-c") 'org-clock-prefix)
(define-key org-clock-prefix (kbd "C-x") 'org-clock-in-last)
(define-key org-clock-prefix (kbd "C-j") 'org-clock-goto)
(define-key org-clock-prefix (kbd "C-o") 'org-clock-out)
(define-key org-clock-prefix (kbd "C-k") 'org-clock-cancel)


(provide 'init-org-keybindings)

;;; init-org-keybindings.el ends here
