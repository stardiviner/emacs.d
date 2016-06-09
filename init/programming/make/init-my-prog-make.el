;;; init-my-prog-make.el --- init for Make utility.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Make ]

;;; [ make-mode ]

(use-package make-mode
  :ensure t)


;;; [ CMake ]

(require 'init-my-prog-make-cmake)


(provide 'init-my-prog-make)

;;; init-my-prog-make.el ends here
