;;; init-org-face.el --- init for Org Faces
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-agenda-mode-hook 'variable-pitch-mode)

;; Date
;; Date: Saturday   27 July 2013
(set-face-attribute 'org-date nil
                    :inherit 'fixed-pitch
                    :background (color-darken-name (face-background 'default) 5)
                    :box '(:color "black" :line-width -1 :style nil)
                    :underline nil)
(set-face-attribute 'org-date-selected nil
                    :inherit 'fixed-pitch)
(set-face-attribute 'org-agenda-date nil
                    :inherit 'fixed-pitch
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 10))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 5)))
                    :box '(:color "gray" :line-width 3 :style nil)
                    :weight 'bold)
(set-face-attribute 'org-agenda-date-weekend nil
                    :inherit 'fixed-pitch
                    :foreground "deep pink"
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 10))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 5)))
                    :box '(:color "dim gray" :line-width 3 :style nil)
                    :weight 'bold)
(set-face-attribute 'org-agenda-date-today nil
                    :inherit 'fixed-pitch
                    :foreground "cyan"
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 10))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 5)))
                    :box '(:color "dim gray" :line-width 5 :style nil)
                    :weight 'bold)
(set-face-attribute 'org-agenda-current-time nil
                    :inherit 'fixed-pitch
                    :foreground (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light "deep pink")
                                  ('dark "cyan"))
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 10))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 5)))
                    :box '(:line-width 1)
                    :underline nil
                    :bold t)

;; Daily entry (holidays)
(set-face-attribute 'org-agenda-diary nil
                    :inherit 'fixed-pitch
                    :slant 'italic
                    :family "Comic Sans MS"
                    :foreground "orange")

;; Clocking
(set-face-attribute 'org-clock-overlay nil
                    :inherit 'fixed-pitch
                    :inverse-video nil
                    :weight 'bold
                    :foreground "white" :background "#004A5D"
                    :box '(:color "cyan" :line-width -1 :style nil))
(set-face-attribute 'org-agenda-clocking nil
                    :inherit 'fixed-pitch
                    :weight 'bold
                    :foreground (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light "black")
                                  ('dark "white"))
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light "green yellow")
                                  ('dark "cyan")))

;; Day-agenda (W30) -> Week number
(set-face-attribute 'org-agenda-structure nil
                    :inherit 'fixed-pitch
                    :weight 'extra-bold)
(set-face-attribute 'org-agenda-filter-tags nil
                    )
(set-face-attribute 'org-agenda-dimmed-todo-face nil
                    :inherit 'fixed-pitch
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
                    :inherit 'fixed-pitch
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
                         :background "OrangeRed"
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
;;                     :box '(:color "dim gray" :line-width 3 :style released-button)
;;                     :bold nil)

;;; Agenda Time Grid
;; time grid: 18:00 ...... ----------------
(set-face-attribute 'org-time-grid nil
                    :inherit 'fixed-pitch
                    :foreground (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-foreground 'default) 5))
                                  ('dark
                                   (color-lighten-name (face-foreground 'default) 10)))
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
(set-face-attribute 'org-upcoming-distant-deadline nil
                    :foreground (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light "orange1")
                                  ('dark "dark orange"))
                    )
;; scheduled in agenda, scheduled today
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
                                  ('light "forest green")
                                  ('dark "olive drab")))
(require 'org-habit)
;; org-habit
(set-face-attribute 'org-habit-overdue-face nil
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 4))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 5))))
(set-face-attribute 'org-habit-overdue-future-face nil
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 7))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 8))))

;; Emphasize
;; org-verbatim: =verbatim=
(set-face-attribute 'org-verbatim nil
                    :inherit 'fixed-pitch
                    :family "Consolas" :height 100)

;; table
(set-face-attribute 'org-table nil
                    :inherit 'fixed-pitch
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 5))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 5))))
;; (set-face-attribute 'org-column nil
;;                     :height (face-attribute 'default :height)
;;                     :family (face-attribute 'default :family))

;; #+TITLE:
(set-face-attribute 'org-document-title nil
                    :inherit nil
                    :weight 'bold
                    :height 1.5
                    :underline t)
;;; #+STARTUP:, #+OPTIONS:
(set-face-attribute 'org-document-info nil
                    :inherit 'fixed-pitch)
(set-face-attribute 'org-document-info-keyword nil
                    :inherit '(shadow fixed-pitch))

;; headline faces
;; the ahead stars face when org indentation with `org-hide'.
(require 'org-indent)
(set-face-attribute 'org-hide nil
                    :inherit 'fixed-pitch
                    :foreground (face-background 'default)
                    :background (face-background 'default))

(setq org-fontify-whole-heading-line t)
(set-face-attribute 'org-level-1 nil
                    :inherit nil
                    :family "Comic Sans MS"
                    :weight 'bold :height 130
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 5))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 3)))
                    )
(set-face-attribute 'org-level-2 nil
                    :inherit 'org-level-1
                    :family "Comic Neue"
                    :height 120
                    )
(set-face-attribute 'org-level-3 nil
                    :inherit 'org-level-2
                    :family (face-attribute 'default :family)
                    :height 110
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

(setq org-fontify-done-headline t)
(set-face-attribute 'org-headline-done nil
                    :foreground (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 20))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 15)))
                    :background (face-background 'default))

;; ellipsis
(set-face-attribute 'org-ellipsis nil
                    :inherit 'fixed-pitch
                    :foreground "DarkRed"
                    :weight 'bold
                    :underline nil)

;; tags

;;; NOTE: It is already defined in `org-tag-faces'.

(set-face-attribute 'org-tag nil
                    :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-tag-group nil
                    :inherit '(shadow fixed-pitch))

;; (set-face-attribute 'org-tag nil
;;                     :background (cl-case (alist-get 'background-mode (frame-parameters))
;;                                   ('light
;;                                    (color-darken-name (face-background 'default) 4))
;;                                   ('dark
;;                                    (color-lighten-name (face-background 'default) 5)))
;;                     :underline nil :weight 'normal :slant 'normal
;;                     :height 0.8
;;                     )

;; (set-face-attribute 'org-tag-group nil
;;                     :background (cl-case (alist-get 'background-mode (frame-parameters))
;;                                   ('light
;;                                    (color-darken-name (face-background 'default) 4))
;;                                   ('dark
;;                                    (color-lighten-name (face-background 'default) 5)))
;;                     :box '(:color "black" :line-width -1)
;;                     )


;;; checkbox faces
;; - [ ], - [X]
(set-face-attribute 'org-checkbox nil
                    :inherit 'fixed-pitch
                    :weight 'normal
                    :box '(:line-width -1 :color "black" :style nil))
;; * headline [7%] [1/10] -> checkbox statistics face.
(set-face-attribute 'org-checkbox-statistics-todo nil
                    :inherit 'fixed-pitch
                    ;; :box '(:color "black" :line-width -1)
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 10))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 5))))
;; * headline [100%] [10/10]
(set-face-attribute 'org-checkbox-statistics-done nil
                    :inherit 'fixed-pitch
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
                    :inherit 'fixed-pitch
                    :weight 'bold)

;;; link face [[link][desc]]
(set-face-attribute 'org-link nil
                    :inherit 'fixed-pitch
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 10))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 5)))
                    :underline "DarkTurquoise")

;; <<<radio target link>>>
(set-face-attribute 'org-target nil
                    :inherit 'fixed-pitch
                    :underline "DimGrey"
                    :weight 'bold)


;; property
;; special keywords :keyword:
(set-face-attribute 'org-special-keyword nil
                    :inherit '(shadow fixed-pitch)
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 10))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 5))))
;; meta lines :PROPERTY: value
(set-face-attribute 'org-meta-line nil
                    :inherit '(font-lock-comment-face fixed-pitch)
                    :background (face-background 'default))
(set-face-attribute 'org-property-value nil
                    :inherit 'fixed-pitch
                    )


;;; babel faces (source code block) => #+BEGIN_SRC ... #+END_SRC
(set-face-attribute 'org-block-begin-line nil
                    :inherit 'fixed-pitch
                    :underline nil
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 4))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 15)))
                    )
(set-face-attribute 'org-block-end-line nil
                    :inherit 'fixed-pitch
                    :overline nil
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 4))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 15)))
                    )
(set-face-attribute 'org-block nil
                    :inherit 'fixed-pitch
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 2))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 3)))
                    )

;;; #+BEGIN_QUOTE
(set-face-attribute 'org-quote nil
                    :inherit 'fixed-pitch
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 10))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 5))))

;; Basic face for displaying the secondary selection.
;; face for babel src block background color when [C-c '] `org-edit-special'.
(set-face-attribute 'secondary-selection nil
                    :inherit 'fixed-pitch
                    :weight 'normal
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 10))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 5))))

;; inline code face => ~code~,  #+RESULTS: : result.
(set-face-attribute 'org-code nil
                    :inherit 'fixed-pitch
                    :family "Consolas" :height 100
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 7))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 3)))
                    :foreground (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light "tomato")
                                  ('dark "cyan"))
                    )

;;; LaTeX
(set-face-attribute 'org-latex-and-related nil
                    :inherit 'fixed-pitch
                    :foreground (color-darken-name (face-foreground 'font-lock-constant-face) 10)
                    )

;;; Formula face
(set-face-attribute 'org-formula nil
                    :inherit 'fixed-pitch
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light "green yellow")
                                  ('dark "green"))
                    :foreground "black"
                    :box '(:color "dim gray" :line-width 1 :style nil)
                    )


(provide 'init-org-face)

;;; init-org-face.el ends here
