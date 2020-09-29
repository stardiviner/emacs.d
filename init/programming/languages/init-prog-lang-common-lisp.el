;;; init-prog-lang-common-lisp.el --- init Common Lisp for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Common Lisp ]

(add-to-list 'auto-mode-alist '("\\.cl\\'" . common-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.lisp\\'" . common-lisp-mode))

;;; re-define upstream default function `lispdoc' key binding.
;; (define-key lisp-mode-map (kbd "C-h d") 'lispdoc)

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

;;; [ cl-font-lock ] -- Pretty Common Lisp font locking.

(use-package cl-font-lock
  :ensure t)

;;; [ ob-lisp ]

(use-package ob-lisp
  :defer t
  :commands (org-babel-execute:lisp)
  :config
  (add-to-list 'org-babel-load-languages '(lisp . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("lisp" . "lisp"))

  (add-to-list 'org-babel-default-header-args:lisp
               '(:noweb . "yes"))
  ;; (add-to-list 'org-babel-default-header-args:lisp
  ;;              '(:results . "output pp"))
  )

;;; [ SBCL ]

(use-package lisp-mode
  ;; the SBCL configuration file is in Common Lisp
  :mode ("\\.sbclrc\\'" . lisp-mode)
  :custom (inferior-lisp-program "sbcl"))

;;; [ Quick Lisp ]

;;; Common Lisp support depends on SLIME being installed with Quicklisp
;;
;; (if (file-exists-p (expand-file-name "~/quicklisp/slime-helper.el"))
;;     (load (expand-file-name "~/quicklisp/slime-helper.el"))
;;   (message "%s" "SLIME is not installed. Use Quicklisp to install it."))


;; (require 'init-slime)
(require 'init-sly)


(provide 'init-prog-lang-common-lisp)

;;; init-prog-lang-common-lisp.el ends here
