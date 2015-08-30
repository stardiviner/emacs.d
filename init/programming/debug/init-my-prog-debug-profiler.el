;;; init-my-prog-debug-profiler.el --- init for profiler
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'my-prog-profiler-prefix)
  (define-prefix-command 'my-prog-profiler-prefix))
(define-key my-prog-debug-map (kbd "p") 'my-prog-profiler-prefix)






(provide 'init-my-prog-debug-profiler)

;;; init-my-prog-debug-profiler.el ends here
