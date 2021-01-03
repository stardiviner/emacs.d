;;; init-emacs-performance.el --- init for Emacs Performance
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ concurrent in Emacs (JIT) ]

;; Allow font-lock-mode to do background parsing.

(setq jit-lock-contextually 'syntax-driven
      jit-lock-context-time 2.0
      jit-lock-chunk-size 1000
      jit-lock-defer-time 0
      jit-lock-stealth-time nil
      jit-lock-stealth-load 200)

;;; [ Garbage Collection (GC) ]

(setq gc-cons-threshold (* 8 (expt 10 8))
      gc-cons-percentage 0.6)

;;; [ GCMH: Garbage Collector Hack Magic ]

;; (use-package gcmh
;;   :ensure t
;;   :custom (gcmh-high-cons-threshold #x20000000)
;;   :init (gcmh-mode 1))

;;; performance issue on long line.
(require 'so-long)
(global-so-long-mode 1)


(provide 'init-emacs-performance)

;;; init-emacs-performance.el ends here
