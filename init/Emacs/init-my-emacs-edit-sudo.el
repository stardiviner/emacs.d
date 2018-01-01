;;; init-my-emacs-edit-sudo.el --- init for Sudo Editing
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ sudo-edit ]

(use-package sudo-edit
  :ensure t
  :bind ("C-x C-r" . sudo-edit)
  )

;;; [ auto-sudoedit ] -- automatic do sudo by tramp when need root file.

;;; XXX: disable this package because it will auto sudoedit Emacs library files.
;; (use-package auto-sudoedit
;;   :ensure t
;;   :config
;;   (auto-sudoedit-mode 1))


(provide 'init-my-emacs-edit-sudo)

;;; init-my-emacs-edit-sudo.el ends here
