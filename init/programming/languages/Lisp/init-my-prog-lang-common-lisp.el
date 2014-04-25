;;; init-my-prog-lang-common-lisp.el --- init Common Lisp for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(require 'init-my-prog-lang-lisp)


;;; [ Common Lisp ]

;; Open files with .cl extension in lisp-mode
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))




;;; [ SBCL ]

;; the SBCL configuration file is in Common Lisp
(add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))



;;; [ Quick Lisp ]

;; Common Lisp support depends on SLIME being installed with Quicklisp
(if (file-exists-p (expand-file-name "~/quicklisp/slime-helper.el"))
    (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (message "%s" "SLIME is not installed. Use Quicklisp to install it."))



(provide 'init-my-prog-lang-common-lisp)

;;; init-my-prog-lang-common-lisp.el ends here
