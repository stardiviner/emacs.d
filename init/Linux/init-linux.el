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

(add-to-list 'auto-mode-alist '("\\.automount\\'" . systemd-mode))
(add-to-list 'auto-mode-alist '("\\.busname\\'" . systemd-mode))
(add-to-list 'auto-mode-alist '("\\.mount\\'" . systemd-mode))
(add-to-list 'auto-mode-alist '("\\.service\\'" . systemd-mode))
(add-to-list 'auto-mode-alist '("\\.socket\\'" . systemd-mode))
(add-to-list 'auto-mode-alist '("\\.target\\'" . systemd-mode))
(add-to-list 'auto-mode-alist '("\\.timer\\'" . systemd-mode))
(add-to-list 'auto-mode-alist '("\\.link\\'" . systemd-mode))
(add-to-list 'auto-mode-alist '("\\.netdev\\'" . systemd-mode))
(add-to-list 'auto-mode-alist '("\\.network\\'" . systemd-mode))

;; built-in default.
;; (add-hook 'systemd-mode-hook
;;           (lambda ()
;;             (add-to-list (make-local-variable 'company-backends)
;;                          'systemd-company-backend)))


;;; crontab-mode

;; - [C-c C-c] -- finish editing.

(load "~/.emacs.d/init/extensions/crontab-mode.el")

;; (add-to-list 'auto-mode-alist '("\\.cron\\(tab\\)?\\'" . crontab-mode))
(add-to-list 'auto-mode-alist '("cron\\(tab\\)?\\."    . crontab-mode))



(provide 'init-linux)

;;; init-linux.el ends here
