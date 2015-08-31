;;; init-my-prog-lang-common-lisp.el --- init Common Lisp for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Common Lisp ]

;; Open files with .cl extension in lisp-mode
(add-to-list 'auto-mode-alist '("\\.cl\\'" . common-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.lisp\\'" . common-lisp-mode))

;;; re-define upstream default function 'lispdoc key binding.
(define-key lisp-mode-map (kbd "C-h d") 'lispdoc)

(setq cl-lookup-categories
      '(:hyperspec-index           ; e.g. "", "spec" "CLHS"
        :hyperspec-chapters        ; e.g. [index], [syntax]
        :format-control-characters ; e.g. "~C: Character", "~%: Newline"
        :reader-macro-characters   ; e.g. "(", "#'", "#b", "#+"
        :loop                      ; e.g. loop:with, loop:collect
        :arguments                 ; e.g. :test, :key, :eof-error-p
        :concepts                  ; e.g. "lambda lists:", "character names:"
        "cl-lookup-glossary"       ; e.g. {absolute}, {binding}
        "cl-lookup-mop"            ; e.g. add-dependent, ensure-class

        ;; implementation specific categories
        ;; "cl-lookup-clisp"          ; e.g. ext:cd

        ;; library categories
        "cl-lookup-ppcre"          ; e.g. cl-ppcre:parse-tree-synonym
        ))


;;; [ SBCL ]

;; the SBCL configuration file is in Common Lisp
(add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))



;;; [ Quick Lisp ]

;;; Common Lisp support depends on SLIME being installed with Quicklisp
;;
;; (if (file-exists-p (expand-file-name "~/quicklisp/slime-helper.el"))
;;     (load (expand-file-name "~/quicklisp/slime-helper.el"))
;;   (message "%s" "SLIME is not installed. Use Quicklisp to install it."))


;;; [ sly ] -- Sylvester the Cat's Common Lisp IDE

;;; Usage:
;;
;; - [M-x sly] :: fire up SLY and connect to Lisp.
;; - `sly-connect' ::
;; - `sly-mode'

(require 'sly-autoloads)

;; (setq sly-lisp-implementations
;;       '((cmucl ("cmucl" "-quiet"))
;;         ;; (cmucl ("/opt/cmucl/bin/lisp" "-quiet") :init sly-init-command)
;;         (sbcl ("/usr/bin/sbcl") :coding-system utf-8-unix)))

(setq sly-contribs '(sly-fancy sly-retro
                               sly-scratch
                               sly-mrepl
                               sly-autodoc))

(dolist (hook '(sly-mrepl-hook
                sly-mode-hook
                common-lisp-lisp-mode-hook
                lisp-interaction-mode-hook
                ;; emacs-lisp-mode-hook
                ))
  (add-hook hook
            (lambda ()
              (unless (boundp 'lisp-help-doc-map)
                (define-prefix-command 'lisp-help-doc-map))
              (local-set-key (kbd "C-h d") 'lisp-help-doc-map)
              
              (define-key lisp-help-doc-map (kbd "d") 'sly-documentation-lookup)
              )))

(eval-after-load 'sly
  `(define-key sly-prefix-map (kbd "M-h") 'sly-documentation-lookup))

(eval-after-load 'sly-mrepl
  `(define-key sly-mrepl-mode-map (kbd "C-c C-k")
     'sly-mrepl-clear-recent-output))


;;; [ company-sly ] -- Company-mode completion backend for SLY.

(require 'sly-company)

(add-hook 'sly-mode-hook 'sly-company-mode)


;; don't add sly-company backend GLOBALLY.
(setq-default company-backends
              (remq 'sly-company company-backends))

(dolist (hook '(emacs-lisp-mode-hook
                common-lisp-lisp-mode-hook
                lisp-mode-hook
                lisp-interaction-mode-hook
                ielm-mode-hook
                ))
  (add-hook hook
            (lambda ()
              (setq-local company-backends
                          (append '(sly-company) company-backends)))))



(provide 'init-my-prog-lang-common-lisp)

;;; init-my-prog-lang-common-lisp.el ends here
