;;; init-linux.el --- init for Linux
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ systemd-mode ] -- Emacs major mode for editing systemd units.

;;; Usage:
;;
;; Reflects a stripped down conf-mode, except with strict regex for
;; whitespace, and highlighting for special syntax, such as specifiers
;; and booleans.
;;
;; - [C-c C-o] :: open links to documentation in a unit.
;;

(use-package systemd
  :ensure t
  :config
  ;; built-in default.
  ;; (add-hook 'systemd-mode-hook
  ;;           (lambda ()
  ;;             (add-to-list (make-local-variable 'company-backends)
  ;;                          'systemd-company-backend)
  ;;             ))
  )


(provide 'init-linux)

;;; init-linux.el ends here
