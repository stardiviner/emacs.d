;;; init-my-prog-debug-profiler.el --- init for profiler
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'profiler-prefix)
  (define-prefix-command 'profiler-prefix))
(define-key debug-prefix (kbd "p") 'profiler-prefix)





(provide 'init-my-prog-debug-profiler)

;;; init-my-prog-debug-profiler.el ends here
