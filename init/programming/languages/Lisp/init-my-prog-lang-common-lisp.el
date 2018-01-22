;;; init-my-prog-lang-common-lisp.el --- init Common Lisp for Emacs.
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

(add-hook 'lisp-mode-hook #'my-lisp-common-settings)
(add-hook 'lisp-interaction-mode-hook #'my-lisp-repl-common-settings)
(add-hook 'common-lisp-mode-hook #'my-lisp-common-settings)


;;; [ ob-lisp ]

(require 'ob-lisp)

(add-to-list 'org-babel-load-languages '(lisp . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-babel-tangle-lang-exts '("lisp" . "lisp"))

(add-to-list 'org-babel-default-header-args:lisp
             '(:noweb . "yes"))
(add-to-list 'org-babel-default-header-args:lisp
             '(:results . "output"))


;;; [ SBCL ]

(use-package lisp-mode
  ;; the SBCL configuration file is in Common Lisp
  :mode ("\\.sbclrc\\'" . lisp-mode)
  :ensure-system-package sbcl
  )


;;; [ Quick Lisp ]

;;; Common Lisp support depends on SLIME being installed with Quicklisp
;;
;; (if (file-exists-p (expand-file-name "~/quicklisp/slime-helper.el"))
;;     (load (expand-file-name "~/quicklisp/slime-helper.el"))
;;   (message "%s" "SLIME is not installed. Use Quicklisp to install it."))

;;; [ Quickdocs ] -- Library Documentation Hosting for Common Lisp
;; http://quickdocs.org/
(use-package engine-mode
  :ensure t
  :config
  (defengine quickdocs
    "http://quickdocs.org/search?q=%s"
    :docstring "Quickdocs")
  (define-key lisp-mode-map (kbd "C-h d q") 'engine/search-quickdocs))


;; (require 'init-slime)
(require 'init-sly)


(provide 'init-my-prog-lang-common-lisp)

;;; init-my-prog-lang-common-lisp.el ends here
