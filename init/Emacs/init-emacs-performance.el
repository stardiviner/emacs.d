;;; init-emacs-performance.el --- init for Emacs Performance
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ concurrent in Emacs (JIT) ]

;; Allow font-lock-mode to do background parsing.
;; (setq jit-lock-stealth-time 1
;;       ;; jit-lock-stealth-load 200
;;       jit-lock-chunk-size 1000
;;       jit-lock-defer-time 0.05)


;;; [ Garbage Collection ]

;; (setq garbage-collection-messages nil)
;; (setq gc-cons-threshold (* 8 (expt 10 8))
;;       gc-cons-percentage 0.6)

;; (setq file-name-handler-alist nil)


(provide 'init-emacs-performance)

;;; init-emacs-performance.el ends here
