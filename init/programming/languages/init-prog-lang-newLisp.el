;;; init-prog-lang-lisp-newLisp.el --- init for newLisp
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ newlisp-mode ]

(use-package newlisp-mode
  :ensure t
  :defer t
  :preface (define-derived-mode newlisp-mode prog-mode "NewLisp")
  :init
  (add-to-list 'auto-mode-alist '("\\.lsp$" . newlisp-mode))
  (add-to-list 'interpreter-mode-alist '("newlisp" . newlisp-mode))
  :config

  (add-hook 'newlisp-mode-hook
            #'(lambda ()
                (add-hook 'completion-at-point-functions
                          'newlisp-completion-at-point nil t)))
  
  ;; setup `*newlisp*' buffer (`comint-mode' of newLisp)
  (defun newlisp-repl-inferior-buffer-setup ()
    (if (equal (buffer-name) "*newlisp*")
        (progn
          ;; (paredit-mode t)
          (with-eval-after-load 'smartparens
            (if (fboundp 'smartparens-strict-mode)
                (smartparens-strict-mode 1)))
          ;; for `company-mode' backend `company-capf'
          (add-hook 'completion-at-point-functions
                    'newlisp-completion-at-point nil t))))

  (add-hook 'comint-mode-hook 'newlisp-repl-inferior-buffer-setup))


(provide 'init-prog-lang-lisp-newLisp)

;;; init-prog-lang-lisp-newLisp.el ends here
