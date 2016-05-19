;;; init-my-prog-lang-C++.el --- init for C++
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ modern-cpp-font-lock ] -- font-locking for C++ mode.

(use-package modern-cpp-font-lock
  :ensure t
  :config
  (modern-c++-font-lock-global-mode t)
  ;; local for current buffer
  ;; (add-hook 'c++-mode-hook 'modern-c++-font-lock-mode)
  )


(provide 'init-my-prog-lang-C++)

;;; init-my-prog-lang-C++.el ends here
