;;; init-my-prog-lang-C-common.el --- C-like programming languages common init.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ c-mode-common]

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)

              ;; auto-complete
              (add-to-list 'ac-sources 'ac-source-irony)
              ;; irony locally for company-mode
              ;; (make-local-variable 'company-backends)
              ;; (add-to-list 'company-backends 'company-c-headers)
              ;; (add-to-list 'company-backends 'company-irony)
              )))


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

;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-irony))

;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator
;;     std::|
;; (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)



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




(provide 'init-my-prog-lang-C-common)

;;; init-my-prog-lang-C-common.el ends here
