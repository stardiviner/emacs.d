;;; init-my-prog-lang-lisp-newlisp.el --- init for newLisp
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ newlisp-mode ]

(use-package newlisp-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.lsp$" . newlisp-mode))
  (add-to-list 'interpreter-mode-alist '("newlisp" . newlisp-mode))
  :config

  (add-hook 'newlisp-mode-hook
            #'(lambda ()
                (make-local-variable 'completion-at-point-functions)
                (add-to-list 'completion-at-point-functions
                             'newlisp-completion-at-point)

                (paredit-mode 1)
                (eldoc-mode 1)
                ))
  
  ;; setup `*newlisp*' buffer (`comint-mode' of newLisp)
  (defun newlisp-repl-inferior-buffer-setup ()
    (if (equal (buffer-name) "*newlisp*")
        (progn
          (eldoc-mode t)
          (paredit-mode t)

          ;; for `company-mode' backend `company-capf'
          (make-local-variable 'completion-at-point-functions)
          (add-to-list 'completion-at-point-functions
                       'newlisp-completion-at-point)
          )))

  (add-hook 'comint-mode-hook 'newlisp-repl-inferior-buffer-setup)
  )


(provide 'init-my-prog-lang-lisp-newlisp)

;;; init-my-prog-lang-lisp-newlisp.el ends here
