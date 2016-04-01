;;; init-my-prog-make-cmake.el --- init for CMake
;;; -*- coding: utf-8 -*-

;;; Commentary:

;;; http://www.itk.org/Wiki/CMake/Editors/Emacs

;;; Code:

;;; [ cmake-mode ]

;; Add cmake listfile names to the mode list.
(setq auto-mode-alist
	  (append
	   '(("CMakeLists\\.txt\\'" . cmake-mode))
	   '(("\\.cmake\\'" . cmake-mode))
	   auto-mode-alist))

(autoload 'cmake-mode "cmake-mode.el" t)


;;; [ cmake-font-lock ]

(autoload 'cmake-font-lock-activate "cmake-font-lock.el" t)

(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)


;;; [ cmake-ide ]

;; (use-package cmake-ide
;;   :ensure t
;;   :config
;;   (require 'rtags)
;;   (cmake-ide-setup)
;;   )


(provide 'init-my-prog-make-cmake)

;;; init-my-prog-make-cmake.el ends here
