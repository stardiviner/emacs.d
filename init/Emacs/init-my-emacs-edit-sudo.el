;;; init-my-emacs-edit-sudo.el --- init for Sudo Editing
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ sudo-edit ]

(use-package sudo-edit
  :ensure t
  :config
  (global-set-key (kbd "C-x C-r") 'sudo-edit)
  ;; convenient way to always prompt for file to sudo-edit.
  ;; (global-set-key (kbd "C-x C-r") (lambda ()
  ;;                                   (interactive)
  ;;                                   (sudo-edit 4)))
  )


(provide 'init-my-emacs-edit-sudo)

;;; init-my-emacs-edit-sudo.el ends here
