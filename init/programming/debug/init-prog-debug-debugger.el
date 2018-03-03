;;; init-prog-debug-debugger.el --- init for Debugger
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ realgud ] -- A modular GNU Emacs front-end for interacting with external debuggers.

(use-package realgud
  :ensure t
  :defer t)


;;; [ GDB ]

(use-package gdb-mi
  :ensure t
  :defer t
  :config
  (setq gdb-many-windows t
        gdb-show-main t)
  )


(provide 'init-prog-debug-debugger)

;;; init-prog-debug-debugger.el ends here
