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
  :config
  (defun my/company-cmake-setup ()
    (my-company-add-backend-locally 'company-cmake))
  (add-hook 'cmake-mode-hook #'my/company-cmake-setup))

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

(use-package cmake-project
  :ensure t
  :defer t
  :config
  ;; auto enable `cmake-project-mode' in project contains "CMakeLists.txt".
  (defun maybe-cmake-project-mode ()
    (if (or (file-exists-p "CMakeLists.txt")
            (file-exists-p (expand-file-name "CMakeLists.txt" (car (project-roots (project-current))))))
        (cmake-project-mode)))
  
  (add-hook 'c-mode-hook 'maybe-cmake-project-mode)
  (add-hook 'c++-mode-hook 'maybe-cmake-project-mode))


(provide 'init-cmake)

;;; init-cmake.el ends here
