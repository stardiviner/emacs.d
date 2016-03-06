;;; init-my-prog-debug.el --- init Debug for Programming.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'my-prog-debug-map)
  (define-prefix-command 'my-prog-debug-map))
(global-set-key (kbd "C-c d") 'my-prog-debug-map)


(require 'init-my-prog-debug-debugger)
(require 'init-my-prog-debug-profiler)


(provide 'init-my-prog-debug)

;;; init-my-prog-debug.el ends here
