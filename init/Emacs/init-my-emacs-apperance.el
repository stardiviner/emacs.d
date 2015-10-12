;;; init-my-emacs-apperance.el --- my Emacs apperance init

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


;;; [ Menu Bar ]

;; the menu bar is mostly useless as well
;; but removing it under OS X doesn't make much sense

;; (if (eq system-type 'darwin)
;;     ;; (string-match "apple-darwin" system-configuration)
;;     (with-selected-frame 'frame
;;       (if (display-graphic-p)
;;           (modify-frame-parameters 'frame '((menu-bar-lines . 1)))
;;         (modify-frame-parameters 'frame '((menu-bar-lines . 0)))))
;;   (if (fboundp 'menu-bar-mode) (menu-bar-mode -1)))
;;
;; (setq-default imenu-auto-rescan t)


;;; [ Tool Bar ]

;; (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))


;;; [ Scroll Bar ]

;; (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;;
;; ;; smooth scroll
;; (setq scroll-margin 10
;;       scroll-conservatively 100000
;;       scroll-preserve-screen-position 'always ; 1, screen offset, nil, always keep in center of window. 'always, keep [M-v] undo [C-v].
;;       )


;;; [ yascroll ] -- Yet Another Scroll Bar Mode

;; * 'right-fringe' for rendering scroll bar in right-fringe.
;; * 'left-fringe' for rendering scroll bar in left-fringe.
;; * 'text-area' for rendering scroll bar in text area.

;; (setq yascroll:scroll-bar 'right-fringe ; 'right-fringe, 'left-fringe, 'text-area.
;;       ;; yascroll:disabled-modes
;;       yascroll:delay-to-hide nil ; nil, 0.5
;;       ;; yascroll:enabled-window-systems '(nil x w32 ns pc)
;;       )
;;
;; (set-face-attribute 'yascroll:thumb-text-area nil
;;                     :foreground "slate blue")
;;
;; (global-yascroll-bar-mode 1)



;;; [ border ]

;; (set-frame-parameter (selected-frame) 'internal-border-width 1)


;;; [ fringe ]

;; (defconst fringe-styles
;;   '(("default" . nil)
;;     ("no-fringes" . 0)
;;     ("right-only" . (0 . nil))
;;     ("left-only" . (nil . 0))
;;     ("half-width" . (4 . 4))
;;     ("minimal" . (1 . 1)))
;;   "Alist mapping fringe mode names to fringe widths.
;; Each list element has the form (NAME . WIDTH), where NAME is a
;; mnemonic fringe mode name and WIDTH is one of the following:
;; - nil, which means the default width (8 pixels).
;; - a cons cell (LEFT . RIGHT), where LEFT and RIGHT are
;;   respectively the left and right fringe widths in pixels, or
;;   nil (meaning the default width).
;; - a single integer, which specifies the pixel widths of both
;; fringes.")

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


;;; [ modeline ]

(require 'init-my-emacs-mode-line)

;; (require 'init-my-emacs-powerline)


;;; [ echo area ]

(setq echo-keystrokes 0.1) ; faster echo key strokes


;;; [ Frame ]

;;; full screen
;; - [F11] -- fullscreen.
;; - [M-F10] -- max window.


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


;;; [ Layout ]

;; - (info "(elisp) Layout Parameters")

;; (let ((display-table (or standard-display-table (make-display-table))))
;;   (set-display-table-slot display-table 'vertical-border (make-glyph-code ?┃))
;;   (setq standard-display-table display-table))

;; (set-face-attribute 'vertical-border nil
;;                     :width 1)


;;; [ Window ]
(setq window-min-height 4
      window-min-width 10
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
(global-hl-line-mode 1) ; highlight current line

(set-face-attribute 'hl-line nil
                    ;; 1
                    ;; :foreground nil :background nil
                    ;; :weight 'bold :foreground " " :background " "
                    ;; 2
                    ;; :foreground nil :background "#004A5D"
                    ;; :box '(:color "#005D5E" :line-width -1)
                    ;; 3
                    ;; :foreground " " :background "#004A5D"
                    ;; :box '(:color "cyan" :line-width 1 :style nil) :underline nil
                    ;; :underline "yellow" :foreground nil :background nil
                    ;; 4. darker percent 5%
                    :foreground nil :background (color-darken-name (face-background 'default) 3)
                    )

(setq hl-line-face 'hl-line)

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

(setq mouse-avoidance-mode 'animate ; auto move mouse away when cursor is at mouse position
      cursor-in-echo-area nil
      cursor-in-non-selected-windows t
      mouse-yank-at-point t
      blink-cursor-blinks 10
      )

;;; red horizontal bar
;; (setq-default cursor-type '(hbar . 3))
;; (set-cursor-color "red")

;;; transparent hollow
(setq-default cursor-type 'hollow)
(set-cursor-color "green")
(set-face-attribute 'cursor nil
                    :background "green")


;;; [ cursor-chg ] -- change cursor color dynamically

;; (require 'cursor-chg)                   ; load the library

;; ;; cursor-chg.el looks nifty but just FYI I have this simple (and by comparison,
;; ;; probably primitive) snippet in my .emacs. I don’t remember where I got it
;; ;; from but works great for me. Note, the hardwired colors are meant for dark
;; ;; backgrounds.

;; (setq curchg-change-cursor-on-input-method-flag t
;;       curchg-change-cursor-on-overwrite/read-only-flag t
;;       curchg-default-cursor-color "cyan"
;;       curchg-default-cursor-type 'hbar
;;       curchg-idle-cursor-type 'box
;;       curchg-input-method-cursor-color "orange"
;;       curchg-overwrite/read-only-cursor-type 'box)

;; (toggle-cursor-type-when-idle 1) ; Turn on cursor change when Emacs is idle
;; (change-cursor-mode 1) ; Turn on change for overwrite, read-only, and input mode


;;; [ Selection ]

;; transpose (for region mark)
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

;; (auto-fill-mode t)
;;; enable only for text writing modes.
(dolist (hook
         '(text-mode-hook
           org-mode-hook
           markdown-mode-hook))
  (add-hook hook 'turn-on-auto-fill))
;;; auto fill comments but not code in programming modes:
(add-hook 'prog-mode-hook (lambda ()
                            (setq-local comment-auto-fill-only-comments t)
                            ;; -- manually --
                            ;; (auto-fill-mode 1)
                            ;; (set (make-local-variable 'fill-nobreak-predicate)
                            ;;      (lambda ()
                            ;;        (not (eq (get-text-property (point) 'face)
                            ;;               'font-lock-comment-face))))
                            ))


;;; { refill mode }
;; This mode does automatic refilling, all the time.
;; Basically, it hits M-q automatically after changes to the buffer that might
;; normally trigger auto-filling. This prevents you from ever writing anything
;; other than perfectly filled paragraphs in your buffers. If you need to write
;; a table or anything else non-filled, you need to disable Refill mode
;; temporarily.
;; (global-set-key (kbd "C-c q") 'refill-mode)
;; types of fill
;; run those commands on paragraphs, buffer, region etc.
;; - [M-x set-justification-full]
;; - [M-x set-justification-left]
;; - [M-x set-justification-center]
;; - [M-x set-justification-right]
;; - [M-x set-justification-none]


;;; highlight search
(setq search-highlight t
      query-replace-highlight t)


;;; [ syntax highlighting ]

(global-font-lock-mode t)


;;; [ prettify-symbols-mode ]

;; TODO:
;; (setq prettify-symbols-alist '(("lambda" . 955)))

;; (global-prettify-symbols-mode 1)


;;; [ pretty-mode (pretty-mode-plus) ] -- Redisplay parts of the Emacs buffer as pretty symbols.

;; https://github.com/akatov/pretty-mode

(require 'pretty-mode)

(add-to-list 'pretty-supported-modes 'ruby-mode)
(add-to-list 'pretty-supported-modes 'enh-ruby-mode)

;; (setq pretty-default-groups '(:function :greek-capitals :greek-lowercase :equality :ordering :ordering-double :ordering-triple :logic :nil :sets :sets-operations :sets-relations :arrows :arrows-twoheaded :arithmetic :arithmetic-double :punctuation :subscripts :superscripts))
;; (setq pretty-active-groups)
;; (add-to-list 'pretty-active-patterns '(ruby-mode))
;; (add-to-list 'pretty-deactivated-patterns '(ruby-mode))

;; FIXME:
;; (add-to-list 'pretty-patterns '((ruby-mode ("->" . ?λ))))

;;; 1. if you want to set it globally
(global-pretty-mode t)
;;; 2. if you want to set it only for a specific mode
(dolist (hook '(prog-mode-hook
                lisp-mode-hook emacs-lisp-mode-hook scheme-mode-hook
                ruby-mode-hook enh-ruby-mode-hook
                org-mode-hook markdown-mode-hook))
  (add-hook hook 'turn-on-pretty-mode))



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


;;; [ on-screen ]

(use-package on-screen
  :config
  (setq on-screen-auto-update t
        on-screen-delay 10
        on-screen-drawing-threshold 2
        on-screen-fringe-marker-position t
        on-screen-highlighting-to-background-delta 0.05
        on-screen-inverse-flag nil
        on-screen-remove-when-edit t
        on-screen-treat-cut-lines t
        ;; fringe       - graphical markers in the fringe
        ;; shadow       - transparent overlay on the text
        ;; line         - transparent overlay on the confining text lines
        ;; narrow-line  - narrow horizontal lines
        on-screen-highlight-method 'narrow-line
        )
  
  (set-face-attribute 'on-screen-fringe nil
                      :foreground "red")
  (set-face-attribute 'on-screen-narrow-line nil
                      :underline '(:color "white" :style line))
  (set-face-attribute 'on-screen-shadow nil
                      :background "dark gray")
  )


;;; [ Fold ]

;;; [ hs-minor-mode ] -- hide/show

;; FIXME: *ERROR*: Web Mode doesn't support Hideshow Minor Mode.
(add-hook 'prog-mode-hook 'hs-minor-mode)


;;; Disable GUI dialog boxes

(setq use-file-dialog 'nil) ; use mini-buffer for file dialogs
(setq use-dialog-box  'nil) ; use mini-buffer for everything' else..


;;; trailing whitespace

;;; But don't show trailing whitespace in SQLi, inf-ruby etc.
(dolist (hook '(special-mode-hook
                eww-mode
                term-mode-hook
                ;; comint-mode-hook
                compilation-mode-hook
                ;; twittering-mode-hook
                minibuffer-setup-hook))
  (add-hook hook
            (lambda () (setq show-trailing-whitespace nil))))

;; (require 'whitespace-cleanup-mode)
;; (global-whitespace-cleanup-mode t)


;;; [ minimap ] -- implementation of a minimap sidebar, i.e., a smaller display of the current buffer on the left side.

;;; Usage:
;; - [mimimap-create] :: create minimap.
;; - [minimap-kill] :: kill minimap.

;; (require 'minimap)

;; (setq minimap-tag-only nil
;;       ;; minimap-numlines 200
;;       minimap-update-delay 0.1
;;       minimap-always-recenter nil
;;       minimap-recenter-type 'relative
;;       minimap-minimum-width 20
;;       minimap-highlight-line nil          ; highlight current line in minimap.
;;       minimap-width-fraction 0.08       ; percent of current width.
;;       minimap-hide-fringes nil
;;       minimap-hide-scroll-bar t
;;       minimap-window-location 'right
;;       minimap-dedicated-window t        ; whether create a dedicated window.
;;       minimap-recreate-window t
;;       ;; BUG: dive into minimap source code to debug this issue.
;;       minimap-automatically-delete-window t ; disable auto delete minimap window will avoid weird window jumping problem. (which auto weird jump to next window after re-switch back to source code window instead of Org-mode buffer.) So set this option to `nil' will preserve the minimap window.
;;       minimap-major-modes '(prog-mode
;;                             ;; org-mode
;;                             markdown-mode Man-mode
;;                             magit-mode)
;;       minimap-normal-height-faces '(font-lock-function-name-face)
;;       minimap-enlarge-certain-faces 'as-fallback
;;       )

;; (add-hook 'emacs-startup-hook 'minimap-create)


;;; [ stripe-buffer ] -- add stripes to "list" buffers

;; Use different background colors for even and odd lines.
;;
;; With the help of library hl-line-mode yet another color can be used for the current line.

;; FIXME: project is still in alpha.
;; (require 'stripe-buffer)

;; (setq stripe-height
;;       stripe-in-table-regex ; Regex for determining whether a line is part of a
;;                             ; table. Used in stripe-table-mode
;;       )

;; (dolist (hook '(dired-mode-hook
;;                 ))
;;   (add-hook hook 'turn-on-stripe-buffer-mode))

;; (after 'org-mode
;;   (if (featurep 'stripe-buffer)
;;       (add-hook 'org-mode-hook 'turn-on-stripe-table-mode)))


;; (set-face-attribute 'stripe-highlight nil
;;                     :background "Grey7"
;;                     :foreground "Gold4"
;;                     )
;; (set-face-attribute 'stripe-hl-line nil ; color for hl-line, when using stripe-listify-buffer
;;                     )
;; (set-face-attribute 'stripe-highlight-overlays nil
;;                     )



(provide 'init-my-emacs-apperance)

;;; init-my-emacs-apperance.el ends here
