;;; init-prog-programming.el --- init for common Programming
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ prog-mode ]

(dolist (hook '(c-mode-hook
                c++-mode-hook
                ruby-mode-hook
                html-mode-hook
                css-mode-hook
                ))
  (add-hook hook (lambda ()
                   (unless (derived-mode-p 'prog-mode)
                     (run-hooks 'prog-mode-hook)))))


(provide 'init-prog-programming)

;;; init-prog-programming.el ends here
