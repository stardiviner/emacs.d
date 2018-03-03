;;; init-prog-debug.el --- init Debug for Programming.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'debug-prefix)
  (define-prefix-command 'debug-prefix))
(global-set-key (kbd "C-c d") 'debug-prefix)


(require 'init-prog-debug-debugger)
(require 'init-prog-debug-profiler)


(provide 'init-prog-debug)

;;; init-prog-debug.el ends here
