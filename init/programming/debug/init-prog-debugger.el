;;; init-prog-debugger.el --- init for Debugger
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ GDB ]

(use-package gdb-mi
  :ensure t
  :defer t
  :commands (gdb gdb-many-windows)
  :custom ((gdb-many-windows t)
           (gdb-show-main t)))

;;; [ realgud ] -- A modular GNU Emacs front-end for interacting with external debuggers.

(use-package realgud
  :ensure t
  :ensure realgud-lldb
  :defer t)

;;; [ moonshot ] -- Run executable file, debug and build commands on project +projectile, compilation-mode, ivy, realgud.

(use-package moonshot
  :ensure t
  :defer t
  :bind (("C-c x x" . moonshot-run-executable)
         ("C-c x d" . moonshot-run-debugger)
         ("C-c x c" . moonshot-run-runner)))


(provide 'init-prog-debugger)

;;; init-prog-debugger.el ends here
