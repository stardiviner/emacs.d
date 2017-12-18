;;; init-my-org-face-extra.el --- init for Extra Org-mode faces.

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
(setq org-babel-exp-inline-code-template "src_%lang[%switches%flags]{%body}"
      org-babel-inline-result-wrap "=> (~%s~)" ; or "=%s=", "~%s~"
      )

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
    (2 '(:foreground "yellow" :weight 'bold :slant 'italic :underline "dark gray")) ; <name>
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
    (2 '(:foreground "yellow" :weight 'bold :slant 'italic :underline "dark gray")) ; <name>
    (3 '(:foreground "gray" :height 0.6)) ; [<inside header argument>]
    (4 '(:foreground "cyan" :weight 'bold)) ; (<arguments>)
    ))
 'append)

;; call_<name>(arguments)
(font-lock-add-keywords
 'org-mode
 '(("\\(call_\\)\\([^[(]*\\)\\((.*)\\)"
    (1 '(:foreground "orange red" :height 0.6)) ; call_
    (2 '(:foreground "yellow" :weight 'bold :slant 'italic :underline "dark gray")) ; <name>
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



;; @@html:<kbd>...</kbd>@@, <kbd> </kbd>
(defface org-html-kbd-tag
  '((nil (:foreground "cyan" :background "#004A5D"
                      :box '(:color "light gray" :line-width 1)
                      ;; :weight 'bold
                      )))
  "Face for highlight Org-mode html tag @<kbd>...@</kbd> or @@html:<kbd>...</kbd>@@."
  :group 'org-faces)

;; @@html:<kbd>C-h h</kbd>@@
(font-lock-add-keywords
 'org-mode
 '(("@@html:<kbd>\\([^<]*\\)</kbd>@@"
    (1 'org-html-kbd-tag))))

;; @<kbd>C-h h@</kbd>
(font-lock-add-keywords
 'org-mode
 '(("@<kbd>\\([^@]*\\)@</kbd>"
    (1 'org-html-kbd-tag))))

(defun my/org-insert-key ()
  "Insert keybinding code in Org with a keybinding quickly.

In common insert mode or in select region text to press this keybinding \\<C-c k>.
to insert <kbd>..</kbd> (HTML) org =[..]= (Org-mode)."
  (interactive)
  (if (region-active-p)
      (let ((where (cons (region-beginning) (region-end))))
        (insert-pair where "=[" "]="))
    ;; (insert-pair nil "=[" "]=")
    (progn
      (insert "=[]=")
      (backward-char 2)))
  )

(defun my/org-insert-kbd ()
  "Insert literal HTML tag <kbd></kbd>."
  (interactive)
  (if (region-active-p)
      (let ((where (cons (region-beginning) (region-end))))
        (insert-pair where "@@html:<kbd>" "</kbd>@@"))
    (progn
      (insert "@@html:<kbd></kbd>@@ ")
      (backward-char 9)))
  )


;;; Inserting the kbd tag in interactively
(eval-after-load 'ox-html
  ;; If you prefer to use ~ for <code> tags. Replace "code" with
  ;; "verbatim" here, and replace "~" with "=" below.
  '(push '(code . "<kbd>%s</kbd>") org-html-text-markup-alist))

(defun my/insert-key (key)
  "Ask for a KEY then insert its description.
Will work on both `org-mode' and any mode that accepts plain html."
  (interactive "kType key sequence: ")
  (let* ((org-p (derived-mode-p 'org-mode))
         (tag (if org-p
                  ;; "~%s~"
                  "=[%s]="
                ;; "@@html:<kbd>%s</kbd>@@"
                "<kbd>%s</kbd>")))
    (if (null (equal key "\C-m"))
        (insert
         (format tag (help-key-description key nil)))
      ;; If you just hit RET.
      (insert (format tag ""))
      (forward-char (if org-p -2 -6)))))

(define-key org-mode-map (kbd "C-c K") 'my/insert-kbd)
(define-key org-mode-map (kbd "C-c k") 'my/org-insert-key)


(provide 'init-my-org-face-extra)

;;; init-my-org-face-extra.el ends here
