;;; init-my-org-keybindings.el --- init for Org Keybindings
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Keybindings ]

(unless (boundp 'my-org-prefix)
  (define-prefix-command 'my-org-prefix)
  (global-set-key (kbd "C-c o") 'my-org-prefix))

(define-key my-org-prefix (kbd "e")
  (defun my-org-element-at-point ()
    (interactive)
    (org-element-at-point)))

(use-package helm
  :config
  (unless (boundp 'my-org-heading-prefix)
    (define-prefix-command 'my-org-heading-prefix))

  (add-hook
   'org-mode-hook
   (lambda ()
     (local-set-key (kbd "C-c h") 'my-org-heading-prefix)
     (define-key my-org-heading-prefix (kbd "h") 'helm-org-in-buffer-headings)
     (define-key my-org-heading-prefix (kbd "a") 'helm-org-agenda-files-headings)
     ))
  )

(unless (boundp 'my-org-agenda-prefix)
  (define-prefix-command 'my-org-agenda-prefix))
(define-key my-org-prefix (kbd "M-a") 'my-org-agenda-prefix)

(define-key my-org-prefix (kbd "a")
  (defun my-open-org-agenda ()
    (interactive)
    ;; TODO:
    ;; (if (memq (current-buffer) (frame-bufs-buffer-list (selected-frame)))
    ;;     (switch to that window)
    ;;   (my-func/open-and-switch-to-buffer 'org-agenda-list "*Org Agenda*" t))
    (my-func/open-and-switch-to-buffer 'org-agenda-list "*Org Agenda*" t)
    ))

(define-key my-org-agenda-prefix (kbd "a") 'my-open-org-agenda)
(define-key my-org-agenda-prefix (kbd "A") 'org-agenda)
(define-key my-org-agenda-prefix (kbd "t") 'org-todo-list) ; prefix [C-u] to prompt keyword for todo list
(define-key org-mode-map (kbd "C-c o M-a T") 'org-timeline) ; Show a time-sorted view of the entries in the current org file.

(unless (boundp 'my-org-link-prefix)
  (define-prefix-command 'my-org-link-prefix))
(define-key my-org-prefix (kbd "M-l") 'my-org-link-prefix)

(define-key my-org-link-prefix (kbd "L") 'org-insert-link-global)
(define-key my-org-link-prefix (kbd "l") 'org-store-link)
(define-key my-org-link-prefix (kbd "o") 'org-open-at-point-global)



(provide 'init-my-org-keybindings)

;;; init-my-org-keybindings.el ends here
