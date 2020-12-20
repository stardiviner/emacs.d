;;; init-emacs-profiler.el --- init profilers for Emacs.

;;; Time-stamp: <2020-12-20 09:11:08 stardiviner>

;;; Commentary:



;;; Code:

;;; [ profiler ] -- Emacs buolt-in profiler.

(use-package profiler
  :defer t
  :commands (profiler-start profiler-report profiler-stop)
  :custom 
  ;; make profiler report line length longer
  ((profiler-report-cpu-line-format '((70 left)
                                      (24 right ((19 right) (5 right)))))
   (profiler-report-memory-line-format '((70 left)
                                         (19 right ((14 right profiler-format-number) (5 right)))))))

;;; [ elp ] -- Emacs Lisp profiler.

(use-package elp
  :defer t
  :commands (elp-instrument-package elp-instrument-function elp-instrument-list elp-results)
  :init (add-to-list 'display-buffer-alist '("\\*ELP Profiling Results\\*" . (display-buffer-below-selected))))

;;; [ benchmark ] -- support for benchmarking code.

(use-package benchmark
  :defer t
  :commands (benchmark benchmark-run benchmark-run-compiled benchmark-progn))

;;; [ trace ] -- tracing facility for Emacs Lisp functions.

(add-to-list 'display-buffer-alist '("\\*trace-output\\*" . (display-buffer-below-selected)))

;;; [ memory-usage ] -- Analyze the memory usage of Emacs in various ways.

(use-package memory-usage
  :ensure t
  :defer t
  :commands (memory-usage)
  :init (add-to-list 'display-buffer-alist '("^\\*Buffer Details\\*" . (display-buffer-below-selected))))

;;; [ explain-pause-mode ] -- Emacs minor mode that watches for long pauses and reports them.

(use-package explain-pause-mode
  :ensure t
  :defer t
  :commands (explain-pause-mode explain-pause-top)
  ;; :hook (after-init . explain-pause-mode)
  :init (explain-pause-mode -1))


(defmacro +measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06fs" (float-time (time-since time)))))



(provide 'init-emacs-profiler)

;;; init-emacs-profiler.el ends here
