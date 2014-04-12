;;; init-my-prog-lang-C.el --- init for C programming language.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;; [ C-mode ]



;;; [ c-eldoc ]

(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)


;;; [ auto-complete-clang ]

;;; https://github.com/mikeandmore/auto-complete-clang

(require 'auto-complete-clang)

(dolist (hook '(c-mode-hook
                c++-mode-hook
                ))
  (add-hook hook (lambda ()
                   (eval-after-load 'auto-complete
                     (add-to-list 'ac-sources 'auto-complete-clang)
                     (add-to-list 'ac-sources 'ac-c-headers)))))


;;; [ ac-c-headers ]


;;; [ auto-complete-clang-async ]

;; (unless (package-installed-p 'auto-complete-clang-async)
;;   (package-install 'auto-complete-clang-async))
;; (require 'auto-complete-clang-async)


;;; [ gccsense ]

;; (unless (package-installed-p 'gccsense)
;;   (package-install 'gccsense))
;; (require 'gccsense)



(provide 'init-my-prog-lang-C)

;;; init-my-prog-lang-C.el ends here
