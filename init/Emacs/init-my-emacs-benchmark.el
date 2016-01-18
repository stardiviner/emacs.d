;;; init-my-emacs-benchmark.el --- init Benchmark settings for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ benchmark-init.el ] -- Benchmark your Emacs initialization

;;; Usage:
;;
;; - [benchmark-init/activate]
;; - [benchmark-init/deactivate]
;; - [benchmark-init/show-durations-tree]
;; - [benchmark-init/show-durations-tabulated]

(use-package benchmark-init
  :ensure t
  :config
  (benchmark-init/activate)
  )


;;; [ esup ]

;;; Usage:
;;
;; - [M-x esup]

(use-package esup
  :ensure t
  )


(provide 'init-my-emacs-benchmark)

;;; init-my-emacs-benchmark.el ends here
