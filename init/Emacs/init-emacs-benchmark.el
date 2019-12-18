;;; init-emacs-benchmark.el --- init Benchmark settings for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ benchmark-init.el ] -- Benchmark your Emacs initialization

(use-package benchmark-init
  :ensure t
  :commands (benchmark-init/show-durations-tree
             benchmark-init/show-durations-tabulated)
  :init (benchmark-init/activate)
  ;; increase to fix error on `benchmark-init/show-durations-tree'
  :config (setq max-specpdl-size 2500))

;;; [ esup ] -- the Emacs StartUp Profiler

(use-package esup
  :ensure t)


(provide 'init-emacs-benchmark)

;;; init-emacs-benchmark.el ends here
