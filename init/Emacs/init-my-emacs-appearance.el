;;; init-my-emacs-appearance.el --- my Emacs apperance init

;;; Commentary:


;;; Code:

(setq inhibit-startup-message 't)
(setq inhibit-startup-echo-area-message "Hacking happy! stardiviner.")


;;; [ Title ]

;; (setq frame-title-format "Emacs λ %b")

(setq frame-title-format
      '("" invocation-name ": "
        (:eval
         (if (buffer-file-name)
             (abbreviate-file-name (buffer-file-name))
           "%b"))))


;;; [ border ]

;; (set-frame-parameter (selected-frame) 'internal-border-width 1)


;;; [ fringe ]

;; (setq fringe-indicator-alist
;;       '((truncation left-arrow right-arrow)
;;         (continuation left-curly-arrow right-curly-arrow)
;;         (overlay-arrow . right-triangle)
;;         (up . up-arrow)
;;         (down . down-arrow)
;;         (top top-left-angle top-right-angle)
;;         (bottom bottom-left-angle bottom-right-angle top-right-angle top-left-angle)
;;         (top-bottom left-bracket right-bracket top-right-angle top-left-angle)
;;         (empty-line . empty-line)
;;         (unknown . question-mark)))

(setq-default indicate-buffer-boundaries 'left
              indicate-empty-lines t
              indicate-unused-lines nil)


;; both side fringe 10 pixels wide.
;; (fringe-mode 10)
;; make left fringe 10 pixels wide, and right fringe disappear.
;; (fringe-mode '(10 . 0))
;; or
;; (set-fringe-style '(10 . 0))
;; restore the default size
(fringe-mode nil)

(set-face-attribute 'fringe nil
                    :foreground "cyan" :background "#073642"
                    )


;;; [ echo area ]

(setq echo-keystrokes 0.01) ; faster echo key strokes


;;; [ Widget ]

(set-face-attribute 'widget-button nil
                    :weight 'bold)
(set-face-attribute 'widget-inactive nil
                    :inherit 'shadow)
(set-face-attribute 'widget-documentation nil
                    :foreground "lime green")
(set-face-attribute 'widget-button-pressed nil
                    :foreground "red1")
(set-face-attribute 'widget-field nil
                    :inherit 'default
                    :foreground "white"
                    :background "#666666"
                    :box '(:color "white" :line-width 1))
(set-face-attribute 'widget-single-line-field nil
                    :inherit 'widget-field
                    :background "#333333"
                    )


;;; [ line space(spacing) / line height ]
;; - (info "(elisp) Line Height")
;; - (info "(elisp) Layout Parameters")
;; The total height of each display line consists of the height of the
;; contents of the line, plus optional additional vertical line spacing
;; above or below the display line.

;; additional space to put between lines.
(setq-default line-spacing nil)         ; 0.1, 1, 0, nil.


;;; [ line number ]

;; display line numbers in margin
;; Linum: separating line numbers from text
;; (setq linum-format 'dynamic)
;; (setq linum-format "%d ") ; 'dynamic
;; (setq linum-format "%4d \u2502") ; a solid line separator
;; TODO: combine 'dynamic result with \u2502
;; (setq linum-format '(combine 'dynamic "\u2502"))

;; (line-number-mode -1)
;; (column-number-mode -1)

;; disable linum-mode because I display line number in mode line.
;; (global-linum-mode -1)

;; but show line numbers in source code files
;; (add-hook 'prog-mode-hook 'linum-mode)


;;; [ current line & column ]

;; highlight current line
(global-hl-line-mode 1)

(set-face-attribute 'hl-line nil
                    ;; 1
                    ;; :background "#004A5D"
                    ;; :box '(:color "#005D5E" :line-width -1)
                    ;; 2
                    ;; :background "#004A5D"
                    ;; :box '(:color "cyan" :line-width 1 :style nil) :underline nil
                    ;; :underline "yellow"
                    ;; 3. darker percent 5%
                    :background (color-darken-name (face-background 'default) 3)
                    )


;;; [ col-highlight ]

;; (require-package 'col-highlight)
;;
;; (column-highlight-mode 1)
;;
;; (col-highlight-toggle-when-idle 1)
;; (col-highlight-set-interval 6)
;;
;; (set-face-attribute 'col-highlight nil
;;                     :background "dodger blue")



;;; [ point & cursor ]

(blink-cursor-mode 1)

(setq-default mouse-avoidance-mode 'animate ; auto move mouse away when cursor is at mouse position
              cursor-in-echo-area nil
              mouse-yank-at-point t
              blink-cursor-blinks 10
              )

;;; horizontal bar
(setq-default cursor-type '(hbar . 2)
              cursor-in-non-selected-windows t)
(set-cursor-color "red")

;;; hollow
;; (setq-default cursor-type 'hollow
;;               cursor-in-non-selected-windows nil)
;; (set-cursor-color "green")


;;; [ beacon ]

(use-package beacon
  :config
  (beacon-mode 1)
  )


;;; [ Selection ]

(setq transient-mark-mode t)


;;; [ wrap & fill ]

;; default column length (80)
(setq-default fill-column 80)

;; disable soft wrap lines for windows which has smaller width than 80.
(global-visual-line-mode -1) ; soft wrap lines at word boundary


;;; [ fci -- Fill Column Indicator ]

;; (eval-after-load 'fill-column-indicator
;;   (setq fci-rule-width 10)
;;   (setq fci-rule-character ?❚)
;;   ;; (setq fci-rule-character-color "#999999")
;;   (setq fci-dash-pattern 1.00))


;;; [ auto-fill-mode ] -- auto fill paragraphs like hitting [M-q].

;;; global
;; (auto-fill-mode t)

;;; auto fill comments but not code in programming modes:
(add-hook 'prog-mode-hook
          '(lambda ()
             (setq-local comment-auto-fill-only-comments t)
             ))

;;; enable only for text writing modes.
(dolist (hook
         '(text-mode-hook
           org-mode-hook
           markdown-mode-hook))
  (add-hook hook 'turn-on-auto-fill))


;;; [ syntax highlighting ]

(global-font-lock-mode t)


;;; [ prettify-symbols-mode ]

;; TODO:
;; (setq prettify-symbols-alist '(("lambda" . 955)))

;; (global-prettify-symbols-mode 1)


;;; [ pretty-mode ] -- redisplay parts of the Emacs buffer as pretty symbols.

(require 'pretty-mode)

(add-to-list 'pretty-supported-modes 'ruby-mode)
(add-to-list 'pretty-supported-modes 'enh-ruby-mode)

;; TODO:
;; (add-to-list 'pretty-default-groups '(:function))
;; (add-to-list 'pretty-active-groups '(ruby-mode :function))
;; (add-to-list 'pretty-active-patterns '(ruby-mode))
;; (add-to-list 'pretty-patterns '((ruby-mode ("->" . ?λ))))

;;; 1. if you want to set it globally
;; (global-pretty-mode t)
;;; 2. if you want to set it only for a specific mode
;; (dolist (hook '(prog-mode-hook
;;                 ))
;;   (add-hook hook 'turn-on-pretty-mode))



;;; [ pretty-symbols ]

;; (require 'pretty-symbols)
;;
;; (setq pretty-symbol-categories '(lambda relational logical)
;;       pretty-symbol-patterns '())


;;; [ page (^L) ]

;; - <C-x [/]> :: navigate.
;; "^\014",
(setq page-delimiter
      (rx bol ";;;" (not (any "#")) (* not-newline) "\n"
          (* (* blank) (opt ";" (* not-newline)) "\n")))
;; Expanded regexp:
;; "^;;;[^#].*\n\\(?:[[:blank:]]*\\(?:;.*\\)?\n\\)*"
;;
;; The regexp above is a bit special. We’re setting the page delimiter to be a
;; ;;; at the start of a line, plus any number of empty lines or comment lines
;; that follow it (that # part is to exclude ;;;###autoload cookies).

;;; hello
(advice-add #'backward-page :after #'recenter)
(advice-add #'forward-page  :after #'recenter)


;;; [ page-break-lines-mode ] --- page breaks (^L characters) are displayed as a horizontal line of a character.

;;; In Page Break mode, page breaks (^L characters) are displayed as a horizontal line of `page-break-string-char' characters.

(require 'page-break-lines)

(global-page-break-lines-mode t)

(setq page-break-lines-char ?─)


;;; Disable GUI dialog boxes

(setq use-dialog-box nil) ; use mini-buffer for everything' else..


;;; trailing whitespace



(provide 'init-my-emacs-appearance)

;;; init-my-emacs-appearance.el ends here
