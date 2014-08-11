;;; init-my-prog-lang-common-lisp.el --- init for Common Lisp
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


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
      



(provide 'init-my-prog-lang-common-lisp)

;;; init-my-prog-lang-common-lisp.el ends here
