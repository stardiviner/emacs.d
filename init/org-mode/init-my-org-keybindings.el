;;; init-my-org-keybindings.el --- init for Org Keybindings
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Keybindings ]

(defun my-org-element-at-point ()
  (interactive)
  (org-element-at-point))

(define-key my-org-prefix (kbd "SPC") #'my-org-element-at-point)

(unless (boundp 'my-org-agenda-prefix)
  (define-prefix-command 'my-org-agenda-prefix))
(define-key my-org-prefix (kbd "M-a") 'my-org-agenda-prefix)

(defun my-org-agenda-switch ()
  (interactive)
  (my-func/open-and-switch-to-buffer 'org-agenda "*Org Agenda*" t)
  )

(define-key my-org-prefix (kbd "a") 'my-org-agenda-switch)
;; (define-key my-org-prefix (kbd "a") 'org-agenda)


(unless (boundp 'my-org-link-prefix)
  (define-prefix-command 'my-org-link-prefix))
(define-key my-org-prefix (kbd "C-l") 'my-org-link-prefix)

(define-key my-org-link-prefix (kbd "l") 'org-store-link)
(define-key my-org-link-prefix (kbd "C-l") 'org-insert-link-global)
(define-key my-org-link-prefix (kbd "C-o") 'org-open-at-point-global)


(unless (boundp 'my-org-clock-prefix)
  (define-prefix-command 'my-org-clock-prefix))
(define-key my-org-prefix (kbd "C-c") 'my-org-clock-prefix)

(define-key my-org-clock-prefix (kbd "i") 'org-clock-in-last)
(define-key my-org-clock-prefix (kbd "s") 'org-clock-select-task)
(define-key my-org-clock-prefix (kbd "g") 'org-clock-goto)
(define-key my-org-clock-prefix (kbd "j") 'org-clock-jump-to-current-clock)
(define-key my-org-clock-prefix (kbd "o") 'org-clock-out)
(define-key my-org-clock-prefix (kbd "c") 'org-clock-cancel)



(provide 'init-my-org-keybindings)

;;; init-my-org-keybindings.el ends here
