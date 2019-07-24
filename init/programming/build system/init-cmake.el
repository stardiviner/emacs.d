;;; init-cmake.el --- init for CMake
;;; -*- coding: utf-8 -*-

;;; Commentary:

;;; http://www.itk.org/Wiki/CMake/Editors/Emacs

;;; Code:

;;; [ cmake-mode ]

(use-package cmake-mode
  :ensure t
  :ensure cmake-font-lock
  :defer t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :config (add-hook 'cmake-mode-hook
                    #'(lambda ()
                        (require 'company-cmake)
                        (my-company-add-backend-locally 'company-cmake))))

;;; [ eldoc-cmake ] -- Eldoc support for CMake.

(use-package eldoc-cmake
  :ensure t
  :defer t
  :hook (cmake-mode . eldoc-cmake-enable))

;;; [ cmake-ide ]

(use-package cmake-ide
  :ensure t
  :defer t
  :init (cmake-ide-setup))

;;; [ cmake-project ] -- Integrates CMake build process with Emacs.

;; (use-package cmake-project
;;   :ensure t
;;   :defer t
;;   :hook (cmake-mode-hook . cmake-project-mode))


(provide 'init-cmake)

;;; init-cmake.el ends here
