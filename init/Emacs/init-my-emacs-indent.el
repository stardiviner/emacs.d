;;; init-my-emacs-indent.el --- init for Indent.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Tab ]

;;; spaces instead of tabs
(setq-default indent-tabs-mode nil)
;; Tab length
(setq default-tab-width 2
      tab-width 2)

;; only controls how much space a tab character occupies in the buffer.
(setq standard-indent 4)
;; (number-sequence 4 120 4) ; 4 tab-stops
;; (number-sequence 2 120 2) ; 4 tab-stops
(setq-default tab-stop-list (number-sequence 2 120 2))

;; (define-key text-mode-map (kbd "<tab>") 'indent-relative) ; 'indent-relative, 'tab-to-tab-stop,


(provide 'init-my-emacs-indent)

;;; init-my-emacs-indent.el ends here
