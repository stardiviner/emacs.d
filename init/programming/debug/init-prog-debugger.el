;;; init-prog-debugger.el --- init for Debugger
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ GDB ]

(use-package gdb-mi
  :ensure t
  :defer t
  :commands (gdb gdb-many-windows)
  :init (setq gdb-many-windows t
              gdb-show-main t))

;;; [ realgud ] -- A modular GNU Emacs front-end for interacting with external debuggers.

(use-package realgud
  :ensure t
  :requires t)

;;; [ realgud-lldb ] -- realgud front-end to lldb.

(use-package realgud-lldb
  :ensure t)


(provide 'init-prog-debugger)

;;; init-prog-debugger.el ends here