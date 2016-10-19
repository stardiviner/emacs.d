;;; init-my-prog-lang-C++.el --- init for C++
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ modern-cpp-font-lock ] -- font-locking for C++ mode.

(use-package modern-cpp-font-lock
  :ensure t
  :defer t
  :init
  (modern-c++-font-lock-global-mode t)
  )


(provide 'init-my-prog-lang-C++)

;;; init-my-prog-lang-C++.el ends here
