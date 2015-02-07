;;; init-my-prog-lang-C.el --- init for C programming language.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;; [ C-mode ]
(setq c-default-style "linux"
      tab-width 4
      )


;;; [ c-eldoc ]

(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)


;;; [ auto-complete-clang ]

;; (require 'auto-complete-clang)
;; (require 'auto-complete-c-headers)
;;
;; (dolist (hook '(c-mode-hook
;;                 c++-mode-hook
;;                 ))
;;   (add-hook hook (lambda ()
;;                    (eval-after-load 'auto-complete
;;                      (lambda ()
;;                        (add-to-list 'ac-sources 'ac-source-clang)
;;                        (add-to-list 'ac-sources 'ac-source-c-headers))))))


;;; [ auto-complete-c-headers ]


;;; [ auto-complete-clang-async ]

;; (unless (package-installed-p 'auto-complete-clang-async)
;;   (package-install 'auto-complete-clang-async))
;; (require 'auto-complete-clang-async)


;;; [ gccsense ]

;; (unless (package-installed-p 'gccsense)
;;   (package-install 'gccsense))
;; (require 'gccsense)


;;; [ company-c-headers ]


;;; [ rtags ]

;;; https://github.com/Andersbakken/rtags




(provide 'init-my-prog-lang-C)

;;; init-my-prog-lang-C.el ends here
