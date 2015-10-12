;;; init-my-emacs-search-grep.el --- init for grep
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Grep ]


;;; [ helm-grep ]

;; (setq helm-grep-default-command "grep -a -d skip %e -n%cH -e %p %f")

(define-key my-search-prefix (kbd "g") 'grep)


;;; [ replace+.el ]



(provide 'init-my-emacs-search-grep)

;;; init-my-emacs-search-grep.el ends here
