;;; init-prog-build-system.el --- init for Build System.

;;; Commentary:



;;; Code:

;;; [ Make ]

(require 'init-prog-make)

;;; [ CMake ]

(require 'init-prog-cmake)

;;; [ build-helper ] -- Utilities to help build code.

(use-package build-helper
  :ensure t
  :defer t)



(provide 'init-prog-build-system)

;;; init-prog-build-system.el ends here
