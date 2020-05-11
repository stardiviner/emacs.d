;;; init-emacs-profiler.el --- init profilers for Emacs.

;;; Time-stamp: <2020-05-11 19:56:52 stardiviner>

;;; Commentary:



;;; Code:

;;; [ profiler ] -- Emacs buolt-in profiler.

(use-package profiler
  :commands (profiler-start profiler-report profiler-stop)
  :init
  ;; make profiler report line length longer
  (setq profiler-report-cpu-line-format '((70 left)
                                          (24 right ((19 right) (5 right))))
        profiler-report-memory-line-format '((70 left)
                                             (19 right ((14 right profiler-format-number) (5 right))))))

;;; [ elp ] -- Emacs Lisp profiler.

(use-package elp
  :commands (elp-instrument-package
             elp-instrument-function
             elp-instrument-list
             elp-results)
  :init (add-to-list 'display-buffer-alist
                     '("\\*ELP Profiling Results\\*" . (display-buffer-below-selected))))

;;; [ benchmark ] -- support for benchmarking code.

(use-package benchmark
  :commands (benchmark benchmark-run benchmark-run-compiled benchmark-progn))

;;; [ trace ] -- tracing facility for Emacs Lisp functions.

;; (add-to-list 'display-buffer-alist
;;              '("\\*trace-output\\*" . (display-buffer-below-selected)))



(provide 'init-emacs-profiler)

;;; init-emacs-profiler.el ends here
