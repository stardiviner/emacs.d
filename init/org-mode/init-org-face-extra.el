;;; init-org-face-extra.el --- init for Extra Org-mode faces.

;;; Commentary:



;;; Code:


;; inline code face => src_ruby{require 'something'}
;;
;; (REGEXP . FACE)
;;     Highlight REGEXP with FACE
;; (REGEXP N FACE)
;;     Highlight group N in REGEXP with FACE
;; (REGEXP (N1 FACE1) (N2 FACE2) (N3 FACE3) …)
;;     Highlight group Ni in REGEXP with FACEi
;;
;; src_lang{code...}[:header arguments] / NOTE: override by `org-verbatim'.
;; result in following =[result]=

;; src_
(font-lock-add-keywords
 'org-mode
 '(("\\(src_\\)\\([^[{]+\\)\\(\\[:.*\\]\\)\\({\\)\\([^}]*\\)\\(}\\)"
    (1 '(:foreground "black" :weight 'normal :height 0.1)) ; src_ part
    (2 '(:foreground "cyan" :weight 'bold :height 0.8 :box '(:color "light gray"))) ; "lang" part.
    (3 '(:foreground "#555555" :height 0.7)) ; [:header arguments] part.
    (4 '(:foreground "#333333")) ; {
    (5 'org-code) ; "code..." part.
    (6 '(:foreground "#333333")) ; }
    ))
 'append)

;; src without arguments
(font-lock-add-keywords
 'org-mode
 '(("\\(src_\\)\\([^[{]+\\)\\({\\)\\([^}]*\\)\\(}\\)"
    (1 '(:foreground "black" :weight 'normal :height 0.1)) ; src_ part
    (2 '(:foreground "cyan" :weight 'bold :height 0.8 :box '(:color "light gray"))) ; "lang" part.
    (3 '(:foreground "#333333")) ; {
    (4 'org-code) ; "code..." part.
    (5 '(:foreground "#333333")) ; }
    ))
 'append)

;; inline babel call
;; ... call_<name>[<inside header arguments>](<arguments>)[<end header arguments>] ...
(font-lock-add-keywords
 'org-mode
 '(("\\(call_\\)\\([^[(]*\\)\\[\\(.*\\)\\](\\(.*\\))\\[\\(.*\\)\\]"
    ;; "\\(call_\\)\\([^[(]*\\)\\([([][^)]*]\\)+"
    (1 '(:foreground "orange red" :height 0.6)) ; call_
    (2 '(:foreground "yellow" :weight 'bold :underline "dark gray")) ; <name>
    (3 '(:foreground "gray" :height 0.6)) ; [<inside header arguments>]
    (4 '(:foreground "cyan" :weight 'bold)) ; (<arguments>)
    (5 '(:foreground "gray" :height 0.6)) ; [<end header arguments>]
    ))
 'append)

;; call_<name>[<inside header arguments>](<arguments>)
(font-lock-add-keywords
 'org-mode
 '(("\\(call_\\)\\([^[(]*\\)\\[\\(.*\\)\\](\\(.*\\))"
    (1 '(:foreground "orange red" :height 0.6)) ; call_
    (2 '(:foreground "yellow" :weight 'bold :underline "dark gray")) ; <name>
    (3 '(:foreground "gray" :height 0.6)) ; [<inside header argument>]
    (4 '(:foreground "cyan" :weight 'bold)) ; (<arguments>)
    ))
 'append)

;; call_<name>(arguments)
(font-lock-add-keywords
 'org-mode
 '(("\\(call_\\)\\([^[(]*\\)\\((.*)\\)"
    (1 '(:foreground "orange red" :height 0.6)) ; call_
    (2 '(:foreground "yellow" :weight 'bold :underline "dark gray")) ; <name>
    (3 '(:foreground "cyan" :weight 'bold)) ; (<arguments>)
    ))
 'append)

;;; fontify done checkbox items.
(font-lock-add-keywords
 'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
    1
    'org-headline-done prepend))
 'append)


(provide 'init-org-face-extra)

;;; init-org-face-extra.el ends here
