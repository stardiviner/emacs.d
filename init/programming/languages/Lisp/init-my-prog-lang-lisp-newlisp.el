;;; init-my-prog-lang-lisp-newlisp.el --- init for newLisp
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ newlisp-mode ]

(use-package newlisp-mode
  ;; :ensure t
  :config
  
  ;; setup `*newlisp*' buffer (`comint-mode' of newLisp)
  (defun newlisp-repl-inferior-buffer-setup ()
    (if (equal (buffer-name) "*newlisp*")
        (progn
          (eldoc-mode t)
          (paredit-mode t)
          )))

  (add-hook 'comint-mode-hook 'newlisp-repl-inferior-buffer-setup)
  )


(provide 'init-my-prog-lang-lisp-newlisp)

;;; init-my-prog-lang-lisp-newlisp.el ends here
