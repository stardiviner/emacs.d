;;; init-my-org-face.el --- init for Org Faces
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


;; Date
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
      '((?A . (:foreground "gray" :background "dark red"
                           :box '(:color "#222222" :line-width -1)))
        (?B . (:foreground "gray" :background "dark slate blue"
                           :box '(:color "#222222" :line-width -1)))
        (?C . (:foreground "gray" :background "dim gray"
                           :box '(:color "#222222" :line-width -1)))
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
;; org-verbatim: =verbatim=
(set-face-attribute 'org-verbatim nil
                    :background (color-darken-name (face-background 'default) 5)
                    :foreground "LimeGreen"
                    :weight 'bold
                    ;; :family "Comic Sans MS"
                    )

;; table
(set-face-attribute 'org-table nil
                    :foreground "dodger blue"
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
                    :height 1.5)
(setq org-fontify-whole-heading-line t)
(set-face-attribute 'org-level-1 nil
                    :inherit nil
                    :family "DejaVu Sans Mono"
                    :weight 'bold :height 130
                    :foreground "Steelblue4"
                    :background (color-darken-name (face-background 'default) 2)
                    ;; :box '(:color "black" :line-width -1 :style nil)
                    ;; :overline "dark slate gray"
                    )
(set-face-attribute 'org-level-2 nil
                    :inherit 'org-level-1
                    :foreground "yellow4"
                    :height 110
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

;; tags
(set-face-attribute 'org-tag-group nil
                    :foreground "white" :background "dim gray"
                    :box '(:color "black" :line-width 2))
(set-face-attribute 'org-tag nil
                    ;; :foreground "gray"
                    ;; sci-fi style
                    :foreground "cyan"
                    :underline nil :weight 'normal :slant 'normal
                    :box '(:color "dark green" :line-width 2)
                    :height 0.8
                    )

;;; checkbox faces
;; - [ ], - [X]
(set-face-attribute 'org-checkbox nil
                    :weight 'normal
                    :box '(:line-width 1 :color "black" :style nil)
                    :foreground "dark gray")
;; * headline [7%] [1/10] -> checkbox statistics face.
(set-face-attribute 'org-checkbox-statistics-todo nil
                    :foreground "OliveDrab"
                    :background (color-darken-name (face-background 'default) 4)
                    :box '(:color "black" :line-width -1)
                    )
;; * headline [100%] [10/10]
(set-face-attribute 'org-checkbox-statistics-done nil
                    :background "#444444" :foreground "black"
                    :box '(:color "black" :line-width -1)
                    :strike-through t)

;; list definition terms - term :: definition
(set-face-attribute 'org-list-dt nil
                    :foreground "green yellow")

;;; link face [[link][desc]]
(set-face-attribute 'org-link nil
                    :foreground "cyan"
                    :background (color-darken-name (face-background 'default) 5)
                    :underline "dark cyan"
                    ;; :box '(:color "black")
                    )

;; <<radio target link>>
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
;; meta lines
(set-face-attribute 'org-meta-line nil
                    :foreground "dark gray"
                    :background (face-background 'default)
                    )
(set-face-attribute 'org-property-value nil
                    :foreground "dim gray")


;;; Babel, Source Code, Block
;;
;; sci-fi cyan style code block colorscheme
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
(set-face-attribute 'org-block nil
                    :inherit 'shadow
                    ;; :background (face-foreground 'shadow)
                    ;; :background "#202020"
                    )

;; Basic face for displaying the secondary selection.
;; face for babel src block background color when [C-c '] `org-edit-special'.
(set-face-attribute 'secondary-selection nil
                    :background "dark green"
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

;;; Formula face
(set-face-attribute 'org-formula nil
                    :background "green yellow"
                    :foreground "black"
                    :inverse-video nil
                    :box '(:color "green yellow" :line-width 1 :style nil))


(provide 'init-my-org-face)

;;; init-my-org-face.el ends here
