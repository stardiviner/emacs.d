;;; init-my-prog-debug.el --- init Debug for Programming.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'debug-prefix)
  (define-prefix-command 'debug-prefix))
(global-set-key (kbd "C-c d") 'debug-prefix)


(require 'init-my-prog-debug-debugger)
(require 'init-my-prog-debug-profiler)


(provide 'init-my-prog-debug)

;;; init-my-prog-debug.el ends here
