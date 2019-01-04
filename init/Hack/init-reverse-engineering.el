;;; init-reverse-engineering.el --- init for Reverse Engineering

;;; Time-stamp: <2018-12-28 07:32:03 stardiviner>

;;; Commentary:



;;; Code:

;;; [ rmsbolt ] -- An implementation of the godbolt compiler-explorer for Emacs.

(use-package rmsbolt
  :ensure t
  :load (rmsbolt)
  :commands (rmsbolt-mode rmsbolt-compile))



(provide 'init-reverse-engineering)

;;; init-reverse-engineering.el ends here
