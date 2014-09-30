;;; init-linux.el --- init for Linux
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; crontab-mode

;; - [C-c C-c] -- finish editing.

(load "~/.emacs.d/init/extensions/crontab-mode.el")

;; (add-to-list 'auto-mode-alist '("\\.cron\\(tab\\)?\\'" . crontab-mode))
(add-to-list 'auto-mode-alist '("cron\\(tab\\)?\\."    . crontab-mode))



(provide 'init-linux)

;;; init-linux.el ends here
