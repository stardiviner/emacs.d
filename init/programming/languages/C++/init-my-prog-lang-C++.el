;;; init-my-prog-lang-C++.el --- init for C++
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ modern-cpp-font-lock ] -- font-locking for C++ mode.

(use-package modern-cpp-font-lock
  :ensure t
  :init
  (modern-c++-font-lock-global-mode t)
  )


;;; [ ob-c++ ]

(require 'ob-C)

;; (setq org-babel-C++-compiler "g++") ; "g++", "c++", "g++ -v"

(add-to-list 'org-babel-load-languages '(C++ . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-babel-tangle-lang-exts '("C++" . "cpp"))

;; (add-to-list 'org-babel-default-header-args:C++
;;              '(:results . "output"))


(provide 'init-my-prog-lang-C++)

;;; init-my-prog-lang-C++.el ends here
