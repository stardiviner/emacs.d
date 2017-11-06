;;; init-my-emacs-customize.el --- init for Customize
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


;; saving customizations
;; Usage:
;; - (info "(Emacs) Customization")
(setq custom-file "~/.emacs.d/customize.el")

(if (file-exists-p custom-file)
    (load custom-file)
  (shell-command (concat "touch " custom-file)))


(provide 'init-my-emacs-customize)

;;; init-my-emacs-customize.el ends here
