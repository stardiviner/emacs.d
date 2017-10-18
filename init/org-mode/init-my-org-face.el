;;; init-my-org-face.el --- init for Org Faces
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;; Date
;; Date: Saturday   27 July 2013
(set-face-attribute 'org-date nil
                    :background (color-darken-name (face-background 'default) 5)
                    :box '(:color "black" :line-width 1 :style nil)
                    :underline nil)
(set-face-attribute 'org-agenda-date nil
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 10))
                                  ('dark
                                   (color-darken-name (face-background 'default) 5)))
                    :box '(:color "dark cyan" :line-width 3 :style nil)
                    :weight 'bold)
(set-face-attribute 'org-agenda-date-weekend nil
                    :foreground "deep pink"
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 10))
                                  ('dark
                                   (color-darken-name (face-background 'default) 5)))
                    :box '(:color "dark cyan" :line-width 3 :style nil)
                    :weight 'bold)
(set-face-attribute 'org-agenda-date-today nil
                    :foreground "red"
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 10))
                                  ('dark
                                   (color-darken-name (face-background 'default) 5)))
                    :box '(:color "cyan" :line-width 5 :style nil)
                    :weight 'bold)
(set-face-attribute 'org-agenda-current-time nil
                    :foreground (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light "deep pink")
                                  ('dark "cyan"))
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 10))
                                  ('dark
                                   (color-darken-name (face-background 'default) 5)))
                    :box '(:color "deep pink" :line-width 1 :style nil)
                    :underline nil
                    :bold t)

;; Daily entry (holidays)
(set-face-attribute 'org-agenda-diary nil
                    :slant 'italic
                    )

;; Clocking
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
                    :weight 'extra-bold)
(set-face-attribute 'org-agenda-filter-tags nil
                    )
(set-face-attribute 'org-agenda-dimmed-todo-face nil
                    :foreground (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 20))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 5)))
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-lighten-name (face-background 'default) 5))
                                  ('dark
                                   (color-darken-name (face-background 'default) 5)))
                    :strike-through t)

;; DONE (org agenda log state change tasks, )
(set-face-attribute 'org-agenda-done nil
                    :foreground (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 20))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 5)))
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-lighten-name (face-background 'default) 5))
                                  ('dark
                                   (color-darken-name (face-background 'default) 5)))
                    :strike-through t)

;; Priority
(setq org-priority-faces
      '((?A .
            (:foreground "dark gray"
                         :background "red"
                         :box '(:color "dark gray" :line-width -1)))
        (?B .
            (:foreground "dark gray"
                         :background "dark slate blue"
                         :box '(:color "dark gray" :line-width -1)))
        (?C .
            (:foreground "dim gray"
                         :background "gray"
                         :box '(:color "dark gray" :line-width -1)))
        ))
;; (set-face-attribute 'org-priority nil
;;                     :box '(:color "red" :line-width 3 :style released-button)
;;                     :bold nil)

;;; Agenda Time Grid
;; time grid: 18:00 ...... ----------------
(set-face-attribute 'org-time-grid nil
                    )
;; alread past deadline in agenda
(set-face-attribute 'org-warning nil
                    :foreground (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light "orange red")
                                  ('dark "red")))
;; comming deadline in agenda
(set-face-attribute 'org-upcoming-deadline nil
                    :foreground (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light "orange")
                                  ('dark "orange red")))
;; scheduled in agenda, scheduled today, & org-habit
(set-face-attribute 'org-scheduled-today nil
                    :foreground (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light "sea green")
                                  ('dark "light sea green")))
(set-face-attribute 'org-scheduled nil
                    :foreground (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light "forest green")
                                  ('dark "dark green")))
(set-face-attribute 'org-scheduled-previously nil
                    :foreground (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light "light green")
                                  ('dark "olive drab")))

;; Emphasize
;; org-verbatim: =verbatim=
(set-face-attribute 'org-verbatim nil
                    ;; :slant 'italic
                    :overline t
                    ;; :family "Comic Sans MS"
                    )

;; table
(set-face-attribute 'org-table nil
                    :background (color-darken-name (face-background 'default) 2)
                    )


;; headline faces
;; the ahead stars face when org indentation. (org-hide)
(set-face-attribute 'org-hide nil
                    :foreground (face-background 'default)
                    :background (face-background 'default)
                    )
(set-face-attribute 'org-document-title nil
                    :inherit nil
                    :weight 'bold
                    :height 1.5
                    :underline t)
(setq org-fontify-whole-heading-line t)
(set-face-attribute 'org-level-1 nil
                    :inherit nil
                    ;; :family "Comic Neue"
                    :weight 'bold :height 130
                    :background (color-darken-name (face-background 'default) 2)
                    :overline "dark slate gray"
                    )
(set-face-attribute 'org-level-2 nil
                    :inherit 'org-level-1
                    :height 110
                    )
(set-face-attribute 'org-level-3 nil
                    :inherit 'org-level-2
                    )
(set-face-attribute 'org-level-4 nil
                    :inherit 'org-level-3
                    )
(set-face-attribute 'org-level-5 nil
                    :inherit 'org-level-4
                    )
(set-face-attribute 'org-level-6 nil
                    :inherit 'org-level-5
                    )
(set-face-attribute 'org-level-7 nil
                    :inherit 'org-level-6
                    )
(set-face-attribute 'org-level-8 nil
                    :inherit 'org-level-7
                    )

(set-face-attribute 'org-headline-done nil
                    :foreground (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 4))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 15)))
                    :background (face-background 'default))

;; ellipsis
;; (setq org-ellipsis "...⤵")
(set-face-attribute 'org-ellipsis nil
                    :foreground "red"
                    :weight 'bold
                    :underline nil)

;; tags


;;; checkbox faces
;; - [ ], - [X]
(set-face-attribute 'org-checkbox nil
                    :weight 'normal
                    :box '(:line-width 1 :color "black" :style nil)
                    )
;; * headline [7%] [1/10] -> checkbox statistics face.
(set-face-attribute 'org-checkbox-statistics-todo nil
                    :background (color-darken-name (face-background 'default) 4)
                    ;; :box '(:color "black" :line-width -1)
                    )
;; * headline [100%] [10/10]
(set-face-attribute 'org-checkbox-statistics-done nil
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 4))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 3)))
                    :foreground (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 7))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 6)))
                    :strike-through t)

;; list definition terms - term :: definition
(set-face-attribute 'org-list-dt nil
                    )

;;; link face [[link][desc]]
(set-face-attribute 'org-link nil
                    :background (color-darken-name (face-background 'default) 5)
                    :underline "DarkTurquoise"
                    ;; :box '(:color "black")
                    )

;; <<radio target link>>
(set-face-attribute 'org-target nil
                    :underline "red"
                    :weight 'bold)

;; set Org clock face.
;; That is, make the org-mode-line-clock no longer inherit attributes from the
;; mode-line face. It seems like it gets the attributes from mode-line or
;; mode-line-inactive as appropriate, when displayed in the mode line.
(set-face-attribute 'org-mode-line-clock nil
                    :inherit nil)

;; special keywords
(set-face-attribute 'org-special-keyword nil
                    :background (color-darken-name (face-background 'default) 3)
                    )
;; property
;; meta lines
(set-face-attribute 'org-meta-line nil
                    :background (face-background 'default)
                    )
(set-face-attribute 'org-property-value nil
                    )


;;; babel faces (source code block) => #+BEGIN_SRC ... #+END_SRC
(set-face-attribute 'org-block-begin-line nil
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 4))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 15)))
                    )
(set-face-attribute 'org-block-end-line nil
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 4))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 15)))
                    )
(set-face-attribute 'org-block nil
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 2))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 3)))
                    )

;;; #+BEGIN_QUOTE
;; (set-face-attribute 'org-quote nil
;;                     :background "dim gray")

;; Basic face for displaying the secondary selection.
;; face for babel src block background color when [C-c '] `org-edit-special'.
(set-face-attribute 'secondary-selection nil
                    )

;; inline code face => ~code~,  #+RESULTS: : result.
(set-face-attribute 'org-code nil
                    :inherit nil
                    :family "DejaVu Sans Mono"
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 7))
                                  ('dark
                                   (color-darken-name (face-background 'default) 3)))
                    :foreground (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light "tomato")
                                  ('dark "cyan"))
                    )

;;; LaTeX
;; (set-face-attribute 'org-latex-face nil
;;                     :background "brown"
;;                     :foreground "white"
;;                     )

;;; Formula face
(set-face-attribute 'org-formula nil
                    :inherit nil
                    ;; :background (cl-case (alist-get 'background-mode (frame-parameters))
                    ;;               ('light "green yellow")
                    ;;               ('dark "green"))
                    ;; :foreground "black"
                    ;; :box '(:color "green yellow" :line-width 1 :style nil)
                    )


(provide 'init-my-org-face)

;;; init-my-org-face.el ends here
