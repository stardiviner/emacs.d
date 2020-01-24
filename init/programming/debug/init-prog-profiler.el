;;; init-prog-profiler.el --- init for profiler
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'profiler-prefix)
  (define-prefix-command 'profiler-prefix))
(define-key debug-prefix (kbd "p") 'profiler-prefix)





(provide 'init-prog-profiler)

;;; init-prog-profiler.el ends here
