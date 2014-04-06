;;; init-my-emacs-apperance.el --- my Emacs apperance init

;;; Commentary:


;;; Code:

(setq inhibit-startup-message 't)
(setq inhibit-startup-echo-area-message "Hacking happy! stardiviner.")

;;; [ Title ]
(setq frame-title-format "Emacs λ %b")


;;; [ Tool Bar ]
(tool-bar-mode -1)


;;; [ Menu Bar ]
;; the menu bar is mostly useless as well
;; but removing it under OS X doesn't make much sense
(if (eq system-type 'darwin)
    ;; (string-match "apple-darwin" system-configuration)
    (with-selected-frame 'frame
      (if (display-graphic-p)
          (modify-frame-parameters 'frame '((menu-bar-lines . 1)))
        (modify-frame-parameters 'frame '((menu-bar-lines . 0)))))
  (menu-bar-mode -1))

(setq-default imenu-auto-rescan t)


;;; [ Scroll Bar ]
(scroll-bar-mode -1)

;; smooth scroll
(setq scroll-margin 10
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)


;;; [ modeline ]

(require 'init-my-emacs-modeline)

;; (require 'init-my-emacs-powerline)


;;; [ echo area ]
(setq echo-keystrokes 0.1)


;;; [ Frame ]

;;; full screen
;; - [F11] -- fullscreen.
;; - [M-F10] -- max window.


;;; [ Layout ]

;; - (info "(elisp) Layout Parameters")


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

(setq-default line-spacing 0.1) ; additional space to put between lines.


;;; [ line number ]
(line-number-mode 1)
(column-number-mode 1)

;; display line numbers in margin
;; (global-linum-mode 1) ; disable linum-mode because I display line number in mode line.
;; Linum: separating line numbers from text
;; (setq linum-format "%d ") ; 'dynamic
(setq linum-format "%4d \u2502") ; a solid line separator
;; TODO combine 'dynamic result with \u2502
;; (setq linum-format '(combine 'dynamic "\u2502"))


;;; [ current line & column ]
;; highlight current line
(global-hl-line-mode 1) ; highlight current line
;; disable soft wrap lines for windows which has smaller width than 80.
(global-visual-line-mode -1) ; soft wrap lines at word boundary
(set-face-attribute 'hl-line nil
                    ;; :foreground nil
                    :background "#004A5D"
                    ;; :box '(:color "cyan" :line-width 1 :style nil) :underline nil
                    ;; :underline "cyan" :box nil
                    )
(setq hl-line-face 'hl-line)


;;; [ point & cursor ]
(setq mouse-avoidance-mode 'animate) ;; auto move mouse away when cursor is at mouse position
(blink-cursor-mode 1)
(set-cursor-color "cyan")
(setq-default cursor-type '(bar . 3) ; '(hbar. 3), '(bar . 3), '(box . 2). '(hollow . 2)
              cursor-in-non-selected-windows t)
(setq mouse-yank-at-point t) ; yank at point position instead of mouse position


;;; change cursor color dynamically



;;; [ Selection ]
;; transpose (for region mark)
(setq transient-mark-mode t)
;; (transient-mark-mode 1) ; highlight text selection
;; (delete-selection-mode 1) ; delete selected text when typing


;;; [ wrap & fill ]
;; default column length (80)
(setq-default fill-column 80)


;;; [ auto-fill-mode ] -- auto fill paragraphs like hitting [M-q].
;; (auto-fill-mode t)
;;; enable only for text writing modes.
(dolist (hook
         '(text-mode-hook
           org-mode-hook))
  (add-hook hook 'turn-on-auto-fill))
;;; auto fill comments but not code in programming modes:
(dolist (hook
         '(prog-mode-hook))
  (add-hook hook (lambda ()
                   ;; fill-mode default implement
                   (setq comment-auto-fill-only-comments t)
                   ;; -- manually --
                   ;; (auto-fill-mode 1)
                   ;; (set (make-local-variable 'fill-nobreak-predicate)
                   ;;      (lambda ()
                   ;;        (not (eq (get-text-property (point) 'face)
                   ;;               'font-lock-comment-face))))
                   )))


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


;;; [ Color Theme ]
;; Usage:
;; - [M-x customize-face] -- to custom current point face color/style.
;; - [C-u M-x list-faces-display RET org] -- overview of all the faces in org-mode.
;; - [M-x customize-group org-font-lock] -- custom org-faces and other aspects of org-apperance.
;; - [C-u C-x =] -- verbose information about the properties of the text under the point.
;; - [M-x load-theme RET (theme)] -- load a color-theme.

;;; initialize color-theme
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)))
(setq color-theme-is-global t)

;; load theme way
(add-to-list 'custom-theme-load-path "~/.emacs.d/color-themes/")
;; (load-theme 'color-theme-midnight)


;;; color-theme-solarized

(require 'color-theme-solarized)

(color-theme-solarized-dark)
(setq solarized-termcolors 256
      ;; solarized-degrade t
      solarized-bold t
      solarized-underline t
      solarized-italic t
      solarized-contrast 'normal ; 'normal, 'hight, 'low
      solarized-visibility 'high ; 'normal, 'high, 'low
      ;; solarized-broken-srgb nil    ; nil, t
      )


;;; [ Faces ]
;; italic & bold
(set-face-attribute 'italic nil
                    :slant 'italic
                    :underline nil
                    :foreground "white")
(set-face-attribute 'bold nil
                    :weight 'bold
                    :underline nil
                    :foreground "white")
;; region
(set-face-attribute 'region nil
                    :inverse-video nil
                    :foreground nil
                    :background "#222222"
                    )
;; search
(set-face-attribute 'isearch nil
                    :background "gray"
                    :foreground "black"
                    :weight 'bold
                    :box '(:color "yellow" :line-width 1 :style nil)
                    )
(set-face-attribute 'lazy-highlight nil
                    :background "yellow"
                    :foreground "black"
                    :weight 'bold
                    )
(set-face-attribute 'match nil
                    :background "yellow"
                    :foreground "black"
                    )
(set-face-attribute 'isearch-fail nil
                    :background "red"
                    :foreground "black"
                    :weight 'bold
                    )
;; replace
(set-face-attribute 'query-replace nil
                    :background "cornsilk"
                    :foreground "black"
                    :weight 'bold
                    :box '(:color "deep pink" :line-width 1 :style nil))

;; comment
;; family: DejaVu Serif,  Droid Serif, Gabriola, Gentium, GFS Didot, Latin Modern Mono, Segoe Print,
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic
                    )
;; built-in function.
(set-face-attribute 'font-lock-builtin-face nil
                    :slant 'italic)


;;; highlight search
(setq search-highlight t
      query-replace-highlight t)


;;; [ highlight ]
(global-font-lock-mode t)


;;; [ show-paren-mode ]

(show-paren-mode 1) ; highlight matched parentheses
(setq show-paren-style 'mixed) ; 'parenthesis, 'expression, 'mixed


;;; [ pretty-mode ] -- Redisplay parts of the Emacs buffer as pretty symbols.

;; https://github.com/akatov/pretty-mode

(require 'pretty-mode)

;;; 1. if you want to set it globally
(global-pretty-mode t)
;;; 2. if you want to set it only for a specific mode
;; (dolist (hook '(prog-mode-hook
;;                 lisp-mode-hook emacs-lisp-mode-hook scheme-mode-hook
;;                 ruby-mode-hook))
;;   (add-hook hook 'turn-on-pretty-mode))



;;; [ pretty-symbols ]

;; (require 'pretty-symbols)
;;
;; (setq pretty-symbol-categories '(lambda relational logical)
;;       pretty-symbol-patterns '())


;;; [ page-break-lines-mode ] --- page breaks (^L characters) are displayed as a horizontal line of a character.

;;; In Page Break mode, page breaks (^L characters) are displayed as a horizontal line of `page-break-string-char' characters.

(require 'page-break-lines)

(global-page-break-lines-mode t)
(diminish 'page-break-lines-mode)

(setq page-break-lines-char ?─)

;; 
;; ;;; [ Fold ]

;; ;;; hs-minor-mode --

;; (add-hook 'prog-mode-hook 'hs-minor-mode)



(provide 'init-my-emacs-apperance)

;;; init-my-emacs-apperance.el ends here
