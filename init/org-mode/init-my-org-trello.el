;;; init-my-org-trello.el --- init for Org-Trello
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ org-trello ] -- Sync Trello boards with Emacs' Org-mode buffers.

(use-package org-trello
  :ensure t
  :defer t
  :config
  ;; (setq org-trello-current-prefix-keybinding (kbd "C-c o"))
  (setq org-trello-files '("~/Org/Trello/INBOX.trello.org"))

  ;; auto enable org-trello-mode in org-mode buffers.
  (defun org-trello-mode-auto-enable ()
    "Auto enable org-trello-mode in org-mode."
    (interactive)
    (if (and (equal major-mode 'org-mode)
             (string-match ".*\\.trello\\.org" (buffer-name)))
        (org-trello-mode 1)
      ))
  (add-hook 'org-mode-hook #'org-trello-mode-auto-enable)

  ;; setup default org-trello keybindings
  (add-hook 'org-trello-mode-hook
            (lambda ()
              ;; 1. set default keybindings
              (orgtrello-setup-install-local-prefix-mode-keybinding "C-c t")
              ))

  ;; local `org-todo-keywords' in org-trello buffers.
  ;; (add-hook 'org-trello-mode-hook
  ;;           (lambda ()
  ;;             (setq-local org-todo-keywords
  ;;                         '((sequence "TODO(@/@)" "Doing(g!)" "|" "DONE(d@/!)")))
  ;;             ))
  )


(provide 'init-my-org-trello)

;;; init-my-org-trello.el ends here
