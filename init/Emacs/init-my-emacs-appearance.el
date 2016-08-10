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

(setq-default echo-keystrokes 0.1) ; faster echo key strokes


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

(require 'linum)

;; display line numbers in margin
;; Linum: separating line numbers from text
;; (setq linum-format 'dynamic)
;; (setq linum-format "%d ") ; 'dynamic
(setq linum-format "%4d \u2502") ; a solid line separator
;; combine 'dynamic result with \u2502
;; (setq linum-format '(combine 'dynamic "\u2502"))

;; set line number face
(set-face-attribute 'linum nil
                    :foreground "#666666"
                    ;; :background nil
                    )

;; (line-number-mode -1)
;; (column-number-mode -1)

;; disable linum-mode because I display line number in mode line.
;; (global-linum-mode -1)

;; but show line numbers in source code files
;; (add-hook 'prog-mode-hook 'linum-mode)

;; Turn off linum-mode when file is too big.
;; (add-hook 'prog-mode-hook
;;           (lambda ()
;;             ;; turn off `linum-mode' when there are more than 5000 lines
;;             ;; use `wc -c file' for performance reason
;;             (if (and (executable-find "wc")
;;                      (> (string-to-number (shell-command-to-string (format "wc -c %s" (buffer-file-name))))
;;                         (* 5000 80)))
;;                 (linum-mode -1))))


;;; [ nlinum ] -- show line numbers in margin.

;; (use-package nlinum
;;   :ensure t
;;   :config
;;   (setq nlinum-format "%d ")
;;   )


;;; [ current line & column ]

(set-face-attribute 'highlight nil
                    :inherit nil
                    :foreground nil
                    :background (color-darken-name (face-background 'default) 3)
                    )

;; highlight current line
(global-hl-line-mode 1)

(set-face-attribute 'hl-line nil
                    :inherit nil
                    ;; 1
                    ;; :background "#004A5D"
                    ;; :box '(:color "#005D5E" :line-width -1)
                    ;; 2
                    ;; :background "#004A5D"
                    ;; :box '(:color "cyan" :line-width 1 :style nil) :underline nil
                    ;; :underline "yellow"
                    ;; 3. darker percent 5%
                    ;; :foreground nil
                    ;; :background (color-darken-name (face-background 'default) 3)
                    ;; use `highlight' face.
                    :background (face-background 'highlight)
                    )


;;; [ col-highlight ]

(use-package col-highlight
  :config
  (col-highlight-toggle-when-idle 1)
  (col-highlight-set-interval 6)

  (set-face-attribute 'col-highlight nil
                      :background "dodger blue")
  
  (column-highlight-mode 1)
  )


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
(set-cursor-color "cyan")

;;; hollow
;; (setq-default cursor-type 'hollow
;;               cursor-in-non-selected-windows nil)
;; (set-cursor-color "green")


;;; [ beacon ]

(use-package beacon
  :config
  (beacon-mode 1)
  (setq beacon-color "green yellow")
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


;;; Disable GUI dialog boxes

(setq use-dialog-box nil) ; use mini-buffer for everything' else..


;;; trailing whitespace


;;; Text style

;; (setq text-quoting-style nil)



(provide 'init-my-emacs-appearance)

;;; init-my-emacs-appearance.el ends here
