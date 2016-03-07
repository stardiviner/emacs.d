;;; init-my-emacs-performance.el --- init for Emacs Performance
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ concurrent in Emacs (JIT) ]

;; - [M-x customize-group RET jit-lock RET]

;; (setq-default jit-lock-defer-time nil)
(setq jit-lock-stealth-verbose t)


;;; [ Garbage Collection ]

(setq garbage-collection-messages nil)


(provide 'init-my-emacs-performance)

;;; init-my-emacs-performance.el ends here
