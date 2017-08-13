;;; init-my-prog-make.el --- init for Make utility.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Make ]

;;; [ make-mode ]

(use-package make-mode
  :ensure t
  :defer t)


;;; [ ob-makefile ]

(require 'ob-makefile)

(add-to-list 'org-babel-load-languages '(makefile . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)


;;; [ CMake ]

(require 'init-my-prog-make-cmake)


(provide 'init-my-prog-make)

;;; init-my-prog-make.el ends here
