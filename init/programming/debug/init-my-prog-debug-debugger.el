;;; init-my-prog-debug-debugger.el --- init for Debugger
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ realgud ] -- A modular GNU Emacs front-end for interacting with external debuggers.

(use-package realgud
  :ensure t
  )


;;; [ GDB ]

(use-package gdb-mi
  :ensure t
  :config
  (setq gdb-many-windows t
        gdb-show-main t)
  )


(provide 'init-my-prog-debug-debugger)

;;; init-my-prog-debug-debugger.el ends here
