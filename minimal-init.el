;;; init.el --- Emacs init file.

;;; Commentary:


;;; Code:

;;; [ Debug ]

(setq debug-on-error t
      debug-on-signal nil
      debug-on-quit nil)


;;; [ profiler ]

;; (profiler-start 'cpu+mem)


;;; benchmark

;; (require 'init-my-emacs-benchmark)


;;; some settings for easy usage

(defalias 'yes-or-no-p 'y-or-n-p)


;;; add my init files directory
(let ((default-directory "~/.emacs.d/init/"))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ; shadow
           (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))


;;; [ package.el ]

(load "~/.emacs.d/init/init-my-pm-package.el")
(require 'init-my-pm-package)

(require 'color)
(use-package color-theme
  :ensure t)


;;; my custom functions
(require 'init-my-library)
(require 'init-my-functions)

