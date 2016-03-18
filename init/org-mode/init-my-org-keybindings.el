;;; init-my-org-keybindings.el --- init for Org Keybindings
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Keybindings ]

(define-key my-org-prefix (kbd "e")
  (defun my-org-element-at-point ()
    (interactive)
    (org-element-at-point)))

(unless (boundp 'my-org-agenda-prefix)
  (define-prefix-command 'my-org-agenda-prefix))
(define-key my-org-prefix (kbd "C-a") 'my-org-agenda-prefix)

(define-key my-org-prefix (kbd "a")
  (defun my-open-org-agenda ()
    (interactive)
    ;; TODO:
    ;; (if (memq (current-buffer) (frame-bufs-buffer-list (selected-frame)))
    ;;     (switch to that window)
    ;;   (my-func/open-and-switch-to-buffer 'org-agenda-list "*Org Agenda*" t))
    (my-func/open-and-switch-to-buffer 'org-agenda-list "*Org Agenda*" t)
    ))

(define-key my-org-prefix (kbd "t") 'org-todo-list)

(define-key my-org-agenda-prefix (kbd "a") 'my-open-org-agenda)
(define-key my-org-agenda-prefix (kbd "A") 'org-agenda)
(define-key my-org-agenda-prefix (kbd "t") 'org-todo-list) ; prefix [C-u] to prompt keyword for todo list
(define-key my-org-agenda-prefix (kbd "T") 'org-timeline) ; Show a time-sorted view of the entries in the current org file.

(unless (boundp 'my-org-link-prefix)
  (define-prefix-command 'my-org-link-prefix))
(define-key my-org-prefix (kbd "C-l") 'my-org-link-prefix)

(define-key my-org-link-prefix (kbd "L") 'org-insert-link-global)
(define-key my-org-link-prefix (kbd "l") 'org-store-link)
(define-key my-org-link-prefix (kbd "o") 'org-open-at-point-global)


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
