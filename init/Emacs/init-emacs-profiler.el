;;; init-emacs-profiler.el --- init profilers for Emacs.

;;; Time-stamp: <2020-05-11 06:46:48 stardiviner>

;;; Commentary:



;;; Code:

;;; [ profiler ] -- Emacs buolt-in profiler.

(setq profiler-report-cpu-line-format '((70 left)
                                        (24 right ((19 right) (5 right))))
      profiler-report-memory-line-format '((70 left)
                                           (19 right ((14 right profiler-format-number) (5 right)))))

;;; [ elp ] -- Emacs Lisp profiler.

(use-package elp
  :commands (elp-instrument-package
             elp-instrument-function
             elp-instrument-list
             elp-results)
  :init (add-to-list 'display-buffer-alist
                     '("\\*ELP Profiling Results\\*" . (display-buffer-below-selected))))

;;; [ trace ] -- tracing facility for Emacs Lisp functions.

;; (add-to-list 'display-buffer-alist
;;              '("\\*trace-output\\*" . (display-buffer-below-selected)))



(provide 'init-emacs-profiler)

;;; init-emacs-profiler.el ends here
