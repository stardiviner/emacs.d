;;; init-my-prog-lang-C-common.el --- C-like programming languages common init.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ CC-mode ]

(require 'cc-mode)



;; [ C-mode ]

(setq-default c-syntactic-indentation t
              c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil ; never use tab, always use space only.
              tab-always-indent t ; make tab key always call a indent command.
              c-default-style "linux"
              )

(add-hook 'c-mode-common-hook
          '(lambda ()
             (c-toggle-auto-newline 1)
             ;; (c-toggle-auto-hungry-state 1)

             (electric-indent-mode 1)
             ))

;; How to convert tabs to space in source code?
;; Select a region first, then call `untabify' or `tabify'.


;;; [ c-eldoc ]

(add-hook 'c-mode-common-hook 'turn-on-eldoc-mode)


;;; [ Semantic ]

;; (semantic-mode t)


;;; [ Irony-mode ] --- A C/C++ minor mode for Emacs powered by libclang.

;;; irony-mode is an Emacs minor-mode that aims at improving the editing
;;; experience for the C, C++ and Objective-C languages. It works by using a
;;; combination of an Emacs package and a C++ program (irony-server) that uses
;;; libclang.

;;; On the first run, irony-mode will ask you to build and install
;;; irony-server. To do so, type [M-x irony-install-server RET].

;;; In order to work correctly, irony-mode needs to know the compile
;;; flags. `irony-cdb' aims to provide as automatic as possible compile flags
;;; discovery, with minimal user input.

;;; Usage:
;;
;; - `irony-server' :: irony-server provides the libclang interface to
;;                     irony-mode. It uses a simple protocol based on
;;                     S-expression.
;;
;; On the first run, irony-mode will ask you to build and install
;; irony-server. To do so, type M-x `irony-install-server' RET.
;;
;;
;;; Compilation Database
;;
;; In order to work correctly, irony-mode needs to know the compile
;; flags. irony-cdb aims to provide as automatic as possible compile flags
;; discovery, with minimal user input.
;;
;; Please refer to `irony-cdb-autosetup-compile-options' and
;; `irony-cdb-compilation-databases'.

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

(require 'company-irony)

(add-hook 'irony-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends)
                         'company-irony)
            ;; (optional) adds CC special commands to `company-begin-commands' in order to
            ;; trigger completion at interesting places, such as after scope operator.
            ;;     std::|
            (company-irony-setup-begin-commands)
            ))


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


;;; [ irony-eldoc ]

(eval-after-load 'irony
  '(progn
     (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))


;;; [ flycheck-irony ]

(eval-after-load 'irony
  '(progn
     (add-hook 'irony-mode-hook #'irony-eldoc)))



;;; [ auto-complete-clang ]

;; (require 'auto-complete-clang)
;; (require 'auto-complete-c-headers)
;;
;; (dolist (hook '(c-mode-hook
;;                 c++-mode-hook
;;                 ))
;;   (add-hook hook (lambda ()
;;                    (eval-after-load 'auto-complete
;;                      '(lambda ()
;;                        (add-to-list 'ac-sources 'ac-source-clang)
;;                        (add-to-list 'ac-sources 'ac-source-c-headers))))))


;;; [ auto-complete-c-headers ]


;;; [ auto-complete-clang-async ]

;; (unless (package-installed-p 'auto-complete-clang-async)
;;   (package-install 'auto-complete-clang-async))
;; (require 'auto-complete-clang-async)


;;; [ company-clang ]

;; (setq company-clang-arguments
;;       company-clang-prefix-guesser 'company-clang-guess-prefix
;;       )

(setq company-clang-begin-after-member-access t)


(add-hook 'c-mode-common-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends)
                         '(company-clang company-cmake))))



;;; [ gccsense ]

;; (unless (package-installed-p 'gccsense)
;;   (package-install 'gccsense))
;; (require 'gccsense)


;;; [ company-c-headers ]


;;; [ rtags ]

;;; https://github.com/Andersbakken/rtags



;;; [ function-args ] -- showing an inline arguments hint for the C/C++ function at point.

;; The traditional way of showing function arguments in Emacs is to show them in
;; the minibuffer. This approach isn't optimal, since I have to traverse the
;; whole screen just to see the hint. After that traverse the whole screen back
;; to find the cursor.
;;
;; Other environments such as Qt Creator and Eclipse implement the hint as a
;; popup located exactly where the function call is. This is the behavior that
;; function-args implements for Emacs.
;;
;; Along the way, it fixes the problem of hints for overridden functions by
;; offering to cycle though the available hints.
;;
;; Cursor tracking, i.e. highlighting the current argument in bold and disposing
;; the popup when the point has left the arguments list, is implemented for
;; change hooks only at the moment. This means that you have to type a char in
;; order for the current argument to update.

;;; Usage:
;;
;; - `fa-show' :: Show an overlay hint with current function arguments.
;;
;;    The point position is tracked and the current hint argument is updated
;;    accordingly. After you've called it with M-i, you can cycle the overloaded
;;    functions with M-n/M-h. You can dismiss the hint with M-u or by editing
;;    anywhere outside the function arguments.
;;
;; - `fa-jump'
;;
;;    While the overlay hint is active, jump to the current function. The
;;    default shortcut is M-j. If the overlay isn't active, call whatever was
;;    bound to M-j before (usually it's c-indent-new-comment-line).
;;
;; - `moo-complete'
;; - `moo-propose-virtual'
;; - `moo-propose-override'
;; - `moo-jump-local'
;;
;; - `semantic-force-refresh'


;; (require 'function-args)
;; or
(autoload 'fa-config-default "function-args" nil t)
(autoload 'turn-on-function-args-mode "function-args" nil t)


;;; Put c++-mode as default for *.h files (improves parsing):
;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;;
;;; Enable case-insensitive searching:
;; (set-default 'semantic-case-fold t)
;;
;;; If your includes aren't located in default dirs e.g. /usr/include/ etc, then
;;; you have to do something like this:
;; (semantic-add-system-include "~/Software/deal.II/include/" 'c++-mode)
;; (semantic-add-system-include "/usr/local/boost_1_54_0/" 'c++-mode)
;;
;;; You can add this to improve the parse of macro-heavy code:
;; (require 'semantic/bovine/c)
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file
;;              "/usr/lib/gcc/x86_64-linux-gnu/4.8/include/stddef.h")


(fa-config-default)
;; (add-hook 'c++-mode-hook 'turn-on-function-args-mode)
;; (add-hook 'c-mode-hook 'turn-on-function-args-mode)



(provide 'init-my-prog-lang-C-common)

;;; init-my-prog-lang-C-common.el ends here
