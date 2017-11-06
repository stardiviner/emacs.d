;;; init-my-prog-lang-lisp-newLisp.el --- init for newLisp
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ newlisp-mode ]

(use-package newlisp-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.lsp$" . newlisp-mode))
  (add-to-list 'interpreter-mode-alist '("newlisp" . newlisp-mode))
  :config

  (add-hook 'newlisp-mode-hook
            #'(lambda ()
                (add-hook 'completion-at-point-functions
                          'newlisp-completion-at-point nil t)
                ))
  
  ;; setup `*newlisp*' buffer (`comint-mode' of newLisp)
  (defun newlisp-repl-inferior-buffer-setup ()
    (if (equal (buffer-name) "*newlisp*")
        (progn
          (eldoc-mode t)
          ;; (paredit-mode t)
          (smartparens-strict-mode 1)
          ;; for `company-mode' backend `company-capf'
          (add-hook 'completion-at-point-functions
                    'newlisp-completion-at-point nil t)
          )))

  (add-hook 'comint-mode-hook 'newlisp-repl-inferior-buffer-setup)
  )


(provide 'init-my-prog-lang-lisp-newLisp)

;;; init-my-prog-lang-lisp-newLisp.el ends here
