;;; init-prog-build-system.el --- init for Build System.

;;; Commentary:



;;; Code:

(unless (boundp 'build-system-prefix)
  (define-prefix-command 'build-system-prefix))
(global-set-key (kbd "<f6>") 'build-system-prefix)

;;; [ compile ]

(require 'compile)
(define-key build-system-prefix (kbd "<f8>") 'compile)

;;; [ Make ]

(require 'init-prog-make)

;;; [ CMake ]

(require 'init-prog-cmake)

;;; [ build-helper ] -- Utilities to help build code.

(use-package build-helper
  :ensure t
  :defer t
  :bind (:map build-system-prefix
              ("<f6>" . build-helper-run)
              ("<f7>" . build-helper-re-run)))



(provide 'init-prog-build-system)

;;; init-prog-build-system.el ends here
