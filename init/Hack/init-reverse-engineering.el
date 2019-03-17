;;; init-reverse-engineering.el --- init for Reverse Engineering

;;; Time-stamp: <2019-03-17 16:47:23 stardiviner>

;;; Commentary:



;;; Code:

;;; [ rmsbolt ] -- An implementation of the godbolt compiler-explorer for Emacs.

(use-package rmsbolt
  :ensure t
  :commands (rmsbolt-mode rmsbolt-compile))



(provide 'init-reverse-engineering)

;;; init-reverse-engineering.el ends here
