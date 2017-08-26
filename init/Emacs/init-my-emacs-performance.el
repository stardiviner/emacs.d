;;; init-my-emacs-performance.el --- init for Emacs Performance
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ concurrent in Emacs (JIT) ]

;; - [M-x customize-group RET jit-lock RET]

;; Allow font-lock-mode to do background parsing.
;; (setq jit-lock-stealth-verbose nil
;;       jit-lock-stealth-time 1
;;       ;; jit-lock-stealth-load 200
;;       jit-lock-chunk-size 1000
;;       jit-lock-defer-time 0.05)


;;; [ Garbage Collection ]

;; (setq garbage-collection-messages nil)
;; (setq gc-cons-threshold (* 8 (expt 10 8)))

;; (setq file-name-handler-alist nil)


(provide 'init-my-emacs-performance)

;;; init-my-emacs-performance.el ends here
