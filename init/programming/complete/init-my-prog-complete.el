;;; init-my-prog-complete.el --- init for Programming Completion
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:



;; (require 'init-auto-complete)
(require 'init-company-mode)

;;; make auto-complete work with company-mode
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;               (company-mode -1)
;;               (auto-complete-mode 1)
;;               )))

;; (require 'init-my-prog-complete-ycmd)


(provide 'init-my-prog-complete)

;;; init-my-prog-complete.el ends here
