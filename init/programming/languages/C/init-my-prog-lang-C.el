;;; init-my-prog-lang-C.el --- init for C programming language.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;; [ C-mode ]



;;; [ c-eldoc ]

(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)


;;; [ auto-complete-clang ]

;;; https://github.com/mikeandmore/auto-complete-clang

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

(add-to-list 'company-backends 'company-c-headers)


;;; [ Irony-mode ] --- A C/C++ minor mode for Emacs powered by libclang.


;;; [ irony-mode ]

;;; irony-mode is an Emacs minor-mode that aims at improving the editing
;;; experience for the C, C++ and Objective-C languages. It works by using a
;;; combination of an Emacs package and a C++ program (irony-server) that uses
;;; libclang.

;;; On the first run, irony-mode will ask you to build and install
;;; irony-server. To do so, type [M-x irony-install-server RET].

;;; In order to work correctly, irony-mode needs to know the compile
;;; flags. `irony-cdb' aims to provide as automatic as possible compile flags
;;; discovery, with minimal user input.

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)


;;; [ company-irony ]

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator
;;     std::|
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)



;;; [ ac-irony ]

;; (defun my-ac-irony-setup ()
;;   ;; be cautious, if yas is not enabled before (auto-complete-mode 1), overlays
;;   ;; *may* persist after an expansion.
;;   (yas-minor-mode 1)
;;   (auto-complete-mode 1)
;;
;;   (add-to-list 'ac-sources 'ac-source-irony)
;;   ;; (define-key irony-mode-map (kbd "M-RET") 'ac-complete-irony-async)
;;   )
;;
;; (add-hook 'irony-mode-hook 'my-ac-irony-setup)


;;; [ company-irony ]

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator
;;     std::|
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)



(provide 'init-my-prog-lang-C)

;;; init-my-prog-lang-C.el ends here
