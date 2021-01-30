;;; This file is introduced in Emacs 27.1

;;; Emacs can now be configured using an early init file.
;;;
;;; The file is called 'early-init.el', in 'user-emacs-directory'.  It is
;;; loaded very early in the startup process: before graphical elements
;;; such as the tool bar are initialized, and before the package manager
;;; is initialized.  The primary purpose is to allow customizing how the
;;; package system is initialized given that initialization now happens
;;; before loading the regular init file (see below).

;;; Emacs GUI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;; [ Debug ]
;; for Emacs startup freeze debug.
(setq debug-on-quit t)
(add-hook 'after-init-hook #'(lambda () (setq debug-on-quit nil)))
(setq debug-on-error t)
(add-hook 'after-init-hook #'(lambda () (setq debug-on-error nil)))

;; (setq package-user-dir
;;       (let ((elpa-dir-name (format "elpa_%s" emacs-major-version))) ;default = "elpa"
;;         (file-name-as-directory (expand-file-name elpa-dir-name user-emacs-directory))))

(provide 'early-init)
