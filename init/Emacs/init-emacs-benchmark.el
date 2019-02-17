;;; init-emacs-benchmark.el --- init Benchmark settings for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ benchmark-init.el ] -- Benchmark your Emacs initialization

(use-package benchmark-init
  :ensure t
  :config
  (benchmark-init/activate)
  ;; increase to fix error on `benchmark-init/show-durations-tree'
  (setq max-specpdl-size 2500))


;;; [ esup ] -- the Emacs StartUp Profiler (ESUP)

(use-package esup
  :ensure t
  :defer t
  :commands (esup))


(provide 'init-emacs-benchmark)

;;; init-emacs-benchmark.el ends here
