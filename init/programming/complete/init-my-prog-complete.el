;;; init-my-prog-complete.el --- init for Programming Completion
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ emacs-ycmd ] -- YouCompeleteMe server/daemon

;; (require 'ycmd)

;; (setq ycmd-server-command '("python" "~/compile/Emacs/ycmd"))

;; ;; (set-variable 'ycmd-global-config "/path/to/global_config.py")


;; ;;; for company-mode
;; (if (featurep 'company-mode)
;;     (lambda ()
;;       (require 'company-ycmd)
;;       (company-ycmd-setup)))


(provide 'init-my-prog-complete)

;;; init-my-prog-complete.el ends here
