;;; init-emacs-benchmark.el --- init Benchmark settings for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; Usage: set this variable to non-nil.
(defvar my/emacs-benchmark-toggle nil)

;;; [ benchmark-init.el ] -- Benchmark your Emacs initialization

(use-package benchmark-init
  :if my/emacs-benchmark-toggle
  :ensure t
  :defer t
  :commands (benchmark-init/show-durations-tree benchmark-init/show-durations-tabulated)
  :init (benchmark-init/activate)
  ;; increase to fix error on `benchmark-init/show-durations-tree'
  :custom (max-specpdl-size 2500))

;;; [ esup ] -- the Emacs StartUp Profiler

(use-package esup
  :if my/emacs-benchmark-toggle
  :ensure t
  :defer t
  :commands (esup))


(provide 'init-emacs-benchmark)

;;; init-emacs-benchmark.el ends here
