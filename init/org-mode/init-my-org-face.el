;;; init-my-org-face.el --- init for Org Faces
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


;;; Date
;; Date: Saturday   27 July 2013
(set-face-attribute 'org-date nil
                    :foreground "gray"
                    :background (color-darken-name (face-background 'default) 5)
                    :box '(:color "black" :line-width 1 :style nil)
                    :underline nil)
(set-face-attribute 'org-agenda-date nil
                    :foreground "white" :background "#004A5D"
                    :box '(:color "dark cyan" :line-width 3 :style nil)
                    :weight 'bold)
(set-face-attribute 'org-agenda-date-weekend nil
                    :foreground "deep pink"
                    :background "#222222"
                    :box '(:color "dark cyan" :line-width 3 :style nil)
                    :weight 'bold)
(set-face-attribute 'org-agenda-date-today nil
                    :foreground "white" :background "#004A5D"
                    :box '(:color "cyan" :line-width 5 :style nil)
                    :weight 'bold)
(set-face-attribute 'org-agenda-current-time nil
                    :foreground "cyan" :background "#004A5D"
                    :box '(:color "cyan" :line-width 1 :style nil)
                    :bold nil)
;; Daily entry (holidays)
(set-face-attribute 'org-agenda-diary nil
                    :foreground "light blue"
                    :slant 'italic
                    )
;; clocking
(set-face-attribute 'org-clock-overlay nil
                    :inverse-video nil
                    :foreground "white" :background "#004A5D"
                    :box '(:color "cyan" :line-width 1 :style nil)
                    :bold t)
(set-face-attribute 'org-agenda-clocking nil
                    :foreground "white" :background "#004A5D"
                    :box '(:color "cyan" :line-width 1 :style nil))
;; Day-agenda (W30) -> Week number
(set-face-attribute 'org-agenda-structure nil
                    :foreground "blue"
                    :weight 'extra-bold)
(set-face-attribute 'org-agenda-filter-tags nil
                    :foreground "green yellow")
(set-face-attribute 'org-agenda-dimmed-todo-face nil
                    :foreground "#444444"
                    :background "#222222"
                    :strike-through t)
;; DONE (org agenda log state change tasks, )
(set-face-attribute 'org-agenda-done nil
                    :foreground "#444444"
                    :background "black")
;; Priority
(setq org-priority-faces
      '((?A . (:foreground "white" :background "dark red"
                           :box '(:color "red" :line-width 3 :style released-button)))
        (?B . (:foreground "white" :background "dark slate blue"
                           :box '(:color "white" :line-width 3 :style released-button)))
        (?C . (:foreground "white" :background "dim gray"
                           :box '(:color "dim gray" :line-width 3 :style released-button)))
        ))
;; (set-face-attribute 'org-priority nil
;;                     :box '(:color "red" :line-width 3 :style released-button)
;;                     :bold nil)

;;; Agenda Time Grid
;; time grid: 18:00 ...... ----------------
(set-face-attribute 'org-time-grid nil
                    :foreground "cyan")
;; alread past deadline in agenda
(set-face-attribute 'org-warning nil
                    :foreground "red"
                    :weight 'normal)
;; comming deadline in agenda
(set-face-attribute 'org-upcoming-deadline nil
                    :foreground "OrangeRed")
;; scheduled in agenda, scheduled today, & org-habit
(set-face-attribute 'org-scheduled-today nil
                    :foreground "light sea green")
(set-face-attribute 'org-scheduled nil
                    :foreground "forest green")
(set-face-attribute 'org-scheduled-previously nil
                    :foreground "olive drab")

;; Emphasize
;;; org-verbatim: =verbatim=
(set-face-attribute 'org-verbatim nil
                    :background "#222222" :foreground "orange"
                    :family "Segoe Print"
                    :bold nil :box nil
                    )

;; table
(set-face-attribute 'org-table nil
                    :foreground "dodger blue"
                    :background (color-darken-name (face-background 'default) 2)
                    )


;;; headline faces
;;; the ahead stars face when org indentation. (org-hide)
(set-face-attribute 'org-hide nil
                    :foreground "#002B36" :background "#002B36")
(set-face-attribute 'org-document-title nil
                    :inherit nil
                    :weight 'bold
                    :height 1.5)
(set-face-attribute 'org-level-1 nil
                    :inherit nil
                    :family "DejaVu Sans Mono"
                    :weight 'bold :height 1.3
                    :foreground "Steelblue4"
                    :background (color-darken-name (face-background 'default) 2)
                    ;; :box '(:color "black" :line-width -1 :style nil)
                    :overline "dark slate gray"
                    )
(set-face-attribute 'org-level-2 nil
                    :inherit 'org-level-1
                    :foreground "yellow4"
                    )
(set-face-attribute 'org-level-3 nil
                    :inherit 'org-level-2
                    :foreground "#009E00"
                    )
(set-face-attribute 'org-level-4 nil
                    :inherit 'org-level-3
                    :foreground "cyan"
                    )
(set-face-attribute 'org-level-5 nil
                    :inherit 'org-level-4
                    :foreground "#008080"
                    )
(set-face-attribute 'org-level-6 nil
                    :inherit 'org-level-5
                    :foreground "#166DEF"
                    )
(set-face-attribute 'org-level-7 nil
                    :inherit 'org-level-6
                    :foreground "deep sky blue"
                    )
(set-face-attribute 'org-level-8 nil
                    :inherit 'org-level-7
                    :foreground "chocolate"
                    )
(set-face-attribute 'org-headline-done nil
                    :foreground "#444444")
;; ellipsis
;; (setq org-ellipsis "...⤵")
(set-face-attribute 'org-ellipsis nil
                    :foreground "red"
                    :weight 'bold
                    :underline nil)
;;; tags
(set-face-attribute 'org-tag nil
                    :foreground "cyan"
                    :underline nil :weight 'normal :slant 'normal
                    :box '(:color "dark green" :line-width 2)
                    ;; :height 0.8
                    )
;; meta lines
(set-face-attribute 'org-meta-line nil
                    :foreground "yellow"
                    :background (color-darken-name (face-background 'default) 5)
                    )
;;; checkbox faces
;; - [ ], - [X]
(set-face-attribute 'org-checkbox nil
                    :bold 'normal
                    :box '(:line-width 1 :color "black" :style nil)
                    :foreground "dark gray")
;; * headline [7%] -> checkbox statistics face.
(set-face-attribute 'org-checkbox-statistics-todo nil
                    :box '(:color "black" :line-width -1)
                    :foreground "green yellow"
                    :background (color-darken-name (face-background 'default) 5)
                    )
(set-face-attribute 'org-checkbox-statistics-done nil
                    :background "#444444" :foreground "black"
                    :box '(:color "black" :line-width -1)
                    :strike-through t)
;;; list definition terms
(set-face-attribute 'org-list-dt nil
                    :foreground "green yellow")
;;; link face
(set-face-attribute 'org-link nil
                    :foreground "cyan"
                    :background (color-darken-name (face-background 'default) 5)
                    :underline "dark cyan"
                    ;; :box '(:color "black")
                    )
;; <<target link>>
(set-face-attribute 'org-target nil
                    :foreground "orange" :background "black"
                    :underline "red"
                    :weight 'bold)

;; org structure faces
(set-face-attribute 'org-agenda-structure nil
                    :foreground "gray"
                    :weight 'bold)

;; set Org clock face.
;; That is, make the org-mode-line-clock no longer inherit attributes from the
;; mode-line face. It seems like it gets the attributes from mode-line or
;; mode-line-inactive as appropriate, when displayed in the mode line.
(set-face-attribute 'org-mode-line-clock nil
                    :foreground "cyan"
                    :inherit nil)

;; special keywords
(set-face-attribute 'org-special-keyword nil
                    :foreground "forest green"
                    :background (color-darken-name (face-background 'default) 3)
                    )
;; property
(set-face-attribute 'org-property-value nil
                    :foreground "gray")


;;; Babel, Source Code, Block
;;
;; cyan style code block colorscheme
;; ;;; babel faces (source code block) => #+BEGIN_SRC ... #+END_SRC
;; (set-face-attribute 'org-block-begin-line nil
;;                     :foreground "cyan" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width -1)
;;                     :bold nil :height 80
;;                     )
;; (set-face-attribute 'org-block-end-line nil
;;                     :foreground "cyan" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width -1)
;;                     :bold nil :height 80
;;                     )
;; (set-face-attribute 'org-block nil
;;                     :background "#004A5d"
;;                     )

;;; black style code block colorscheme
;;; babel faces (source code block) => #+BEGIN_SRC ... #+END_SRC
(set-face-attribute 'org-block-begin-line nil
                    :foreground "dark cyan"
                    :background (color-darken-name (face-background 'default) 3)
                    :weight 'normal :slant 'normal
                    :box '(:color "black" :line-width 1)
                    )
(set-face-attribute 'org-block-end-line nil
                    :foreground "dark cyan"
                    :background (color-darken-name (face-background 'default) 3)
                    :weight 'normal :slant 'normal
                    :box '(:color "black" :line-width 1)
                    )

(set-face-attribute 'secondary-selection nil
                    :background (color-darken-name (face-background 'default) 6)
                    )

;; code face => ~code~,  #+RESULTS: : result.
(set-face-attribute 'org-code nil
                    ;; 1. sci-fi cyan style
                    ;; :background "#004A5D" :foreground "white"
                    ;; :box '(:color "cyan" :line-width 1)
                    ;; :family "DejaVu Sans Mono"
                    ;; :underline nil
                    
                    ;; 2. without box, clear for code which has underline.
                    :background "#004A5D" :foreground "white"
                    :family "DejaVu Sans Mono"
                    )

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
    (2 '(:foreground "cyan" :weight 'bold :height 0.8 :box '(:color "cyan"))) ; "lang" part.
    (3 '(:foreground "#555555" :height 0.7)) ; [:header arguments] part.
    (4 '(:foreground "#333333")) ; {
    (5 'org-code) ; "code..." part.
    (6 '(:foreground "#333333")) ; }
    )))

;; src without arguments
(font-lock-add-keywords
 'org-mode
 '(("\\(src_\\)\\([^[{]+\\)\\({\\)\\([^}]*\\)\\(}\\)"
    (1 '(:foreground "black" :weight 'normal :height 0.1)) ; src_ part
    (2 '(:foreground "cyan" :weight 'bold :height 0.8 :box '(:color "cyan"))) ; "lang" part.
    (3 '(:foreground "#333333")) ; {
    (4 'org-code) ; "code..." part.
    (5 '(:foreground "#333333")) ; }
    )))

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
    )))

;; call_<name>[<inside header arguments>](<arguments>)
(font-lock-add-keywords
 'org-mode
 '(("\\(call_\\)\\([^[(]*\\)\\[\\(.*\\)\\](\\(.*\\))"
    (1 '(:foreground "orange red" :height 0.6)) ; call_
    (2 '(:foreground "yellow" :weight 'bold :slant 'italic :underline "dark gray")) ; <name>
    (3 '(:foreground "gray" :height 0.6)) ; [<inside header argument>]
    (4 '(:foreground "cyan" :weight 'bold)) ; (<arguments>)
    )))


;; call_<name>(arguments)
(font-lock-add-keywords
 'org-mode
 '(("\\(call_\\)\\([^[(]*\\)\\((.*)\\)"
    (1 '(:foreground "orange red" :height 0.6)) ; call_
    (2 '(:foreground "yellow" :weight 'bold :slant 'italic :underline "dark gray")) ; <name>
    (3 '(:foreground "cyan" :weight 'bold)) ; (<arguments>)
    )))



;;; Formula face
(set-face-attribute 'org-formula nil
                    :background "green yellow"
                    :foreground "black"
                    :inverse-video nil
                    :box '(:color "green yellow" :line-width 1 :style nil))

;; @@html:<kbd>...</kbd>@@, <kbd> </kbd>
(defface org-html-kbd-tag
  '((nil (:foreground "cyan" :background "#004A5D"
                      :box '(:color "cyan" :line-width 1)
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


;; (defun my/org-insert-kbd (key)
;;   "Ask for a KEY then insert its description.
;; Will work on both `org-mode' and any mode that accepts plain html."
;;   (interactive "kType key sequence: ")
;;   (let* ((is-org-mode (derived-mode-p 'org-mode))
;;          (tag (if is-org-mode
;;                   "@@html:<kbd>@@%s@@html:</kbd>@@"
;;                 "<kbd>%s</kbd>")))
;;     (if (null (equal key "
;; "))
;;         (insert
;;          (format tag (help-key-description key nil)))
;;       (insert (format tag ""))
;;       (forward-char (if is-org-mode -15 -6)))))

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



(provide 'init-my-org-face)

;;; init-my-org-face.el ends here
