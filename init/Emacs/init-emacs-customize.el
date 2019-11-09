;;; init-emacs-customize.el --- init for Customize
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;; Customize Saving

(setq custom-file (expand-file-name "customize.el" user-emacs-directory))

(if (file-exists-p custom-file)
    (load custom-file)
  ;; (use-package f
  ;;   :ensure t
  ;;   :init (f-touch custom-file))
  (shell-command (concat "touch " custom-file)))


(provide 'init-emacs-customize)

;;; init-emacs-customize.el ends here
