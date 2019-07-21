;;; init-emacs-benchmark.el --- init Benchmark settings for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ benchmark-init.el ] -- Benchmark your Emacs initialization

(use-package benchmark-init
  :ensure t
  :init
  ;; 首先下载 benchmark-init 这个插件, 在配置最开始的位置写下配置：
  ;; (let (;; 加载的时候临时增大`gc-cons-threshold'以加速启动速度。
  ;;       (gc-cons-threshold most-positive-fixnum)
  ;;       ;; 清空避免加载远程文件的时候分析文件。
  ;;       (file-name-handler-alist nil))
  ;;   (require 'benchmark-init-modes)
  ;;   (require 'benchmark-init)
  ;;   (benchmark-init/activate)
  ;;   ;; 下面才写你的其它配置
  ;;   )
  ;; 启动完毕后，执行 M-x benchmark-init/show-durations-tree 命令，这个命令会递
  ;; 归的打印出所有插件的耗时明细。
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
