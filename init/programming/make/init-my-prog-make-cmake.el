;;; init-my-prog-make-cmake.el --- init for CMake
;;; -*- coding: utf-8 -*-

;;; Commentary:

;;; http://www.itk.org/Wiki/CMake/Editors/Emacs

;;; Code:

;;; [ cmake-mode ]

(use-package cmake-mode
  :ensure t
  :init
  ;; Add cmake listfile names to the mode list.
  (setq auto-mode-alist
        (append
         '(("CMakeLists\\.txt\\'" . cmake-mode))
         '(("\\.cmake\\'" . cmake-mode))
         auto-mode-alist))
  )


;;; [ cmake-font-lock ]

(use-package cmake-font-lock
  :ensure t
  :config
  (autoload 'cmake-font-lock-activate "cmake-font-lock.el" t)
  (add-hook 'cmake-mode-hook 'cmake-font-lock-activate)
  )


;;; [ cmake-ide ]

(use-package cmake-ide
  :ensure t
  :config
  (cmake-ide-setup)
  )


;;; [ cmake-project ] -- Integrates CMake build process with Emacs.

(use-package cmake-project
  :ensure t)


(provide 'init-my-prog-make-cmake)

;;; init-my-prog-make-cmake.el ends here
