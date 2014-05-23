;;; init-my-prog-debug.el --- init Debug for Programming.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'my-prog-debug-prefix)
  (define-prefix-command 'my-prog-debug-prefix))

;; (global-set-key (kbd "C-c d") 'my-prog-debug-prefix)



(provide 'init-my-prog-debug)

;;; init-my-prog-debug.el ends here
