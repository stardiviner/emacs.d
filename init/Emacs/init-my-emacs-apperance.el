;;; init-my-emacs-apperance.el --- my Emacs apperance init

;;; Commentary:


;;; Code:

(setq inhibit-startup-message 't)
(setq inhibit-startup-echo-area-message "Hacking happy! stardiviner.")


;;; [ Title ]
(setq frame-title-format "Emacs λ %b")


;;; [ Menu Bar ]
;; the menu bar is mostly useless as well
;; but removing it under OS X doesn't make much sense
(if (eq system-type 'darwin)
    ;; (string-match "apple-darwin" system-configuration)
    (with-selected-frame 'frame
      (if (display-graphic-p)
          (modify-frame-parameters 'frame '((menu-bar-lines . 1)))
        (modify-frame-parameters 'frame '((menu-bar-lines . 0)))))
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1)))

(setq-default imenu-auto-rescan t)


;;; [ Tool Bar ]
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))


;;; [ Scroll Bar ]
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

;; smooth scroll
(setq scroll-margin 10
      scroll-conservatively 100000
      scroll-preserve-screen-position 1
      ;; always keep cursor in center of buffer.
      ;; scroll-preserve-screen-position nil
      )


;;; [ yascroll ] -- Yet Another Scroll Bar Mode

;; * 'right-fringe' for rendering scroll bar in right-fringe.
;; * 'left-fringe' for rendering scroll bar in left-fringe.
;; * 'text-area' for rendering scroll bar in text area.

;; (setq yascroll:scroll-bar 'right-fringe ; 'right-fringe, 'left-fringe, 'text-area.
;;       ;; yascroll:disabled-modes
;;       yascroll:delay-to-hide nil ; nil, 0.5
;;       ;; yascroll:enabled-window-systems '(nil x w32 ns pc)
;;       )

;; (set-face-attribute 'yascroll:thumb-text-area nil
;;                     :foreground "slate blue")

;; (global-yascroll-bar-mode t)


;;; [ modeline ]

(require 'init-my-emacs-mode-line)

;; (require 'init-my-emacs-powerline)


;;; [ echo area ]
(setq echo-keystrokes 0.1) ; faster echo key strokes


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

;; additional space to put between lines.
(setq-default line-spacing 0.1)         ; 0.1, 1, 0, nil.


;;; [ line number ]
(line-number-mode 1)
(column-number-mode 1)

;; display line numbers in margin
;; Linum: separating line numbers from text
;; (setq linum-format "%d ") ; 'dynamic
(setq linum-format "%4d \u2502") ; a solid line separator
;; TODO: combine 'dynamic result with \u2502
;; (setq linum-format '(combine 'dynamic "\u2502"))

;; (global-linum-mode 1) ; disable linum-mode because I display line number in mode line.


;;; [ current line & column ]
;; highlight current line
(global-hl-line-mode 1) ; highlight current line
;; disable soft wrap lines for windows which has smaller width than 80.
(global-visual-line-mode -1) ; soft wrap lines at word boundary
(set-face-attribute 'hl-line nil
                    ;; 1
                    ;; :foreground nil :background nil
                    ;; :weight 'bold :foreground " " :background " "
                    ;; 2
                    :foreground nil :background "#004A5D"
                    :box '(:color "#005D5E" :line-width -1)
                    ;; 3
                    ;; :foreground " " :background "#004A5D"
                    ;; :box '(:color "cyan" :line-width 1 :style nil) :underline nil
                    ;; :underline "yellow" :foreground nil :background nil
                    )
(setq hl-line-face 'hl-line)


;;; [ point & cursor ]
(setq mouse-avoidance-mode 'animate) ;; auto move mouse away when cursor is at mouse position
(blink-cursor-mode 1)
(set-cursor-color "cyan")
(setq-default cursor-type '(hbar . 3) ; '(hbar. 3), '(bar . 3), '(box . 2). '(hollow . 2)
              cursor-in-non-selected-windows t)
(setq mouse-yank-at-point t) ; yank at point position instead of mouse position

(setq cursor-in-echo-area nil
      cursor-in-non-selected-windows t)


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
(eval-after-load 'color-theme
  '(progn
     (color-theme-initialize)))
(setq color-theme-is-global t)

;; load theme way
(add-to-list 'custom-theme-load-path "~/.emacs.d/color-themes/")
;; (load-theme 'color-theme-midnight)


;;; color-theme-solarized

(require 'color-theme-solarized)

(color-theme-solarized-dark)
;; (color-theme-solarized-light)

(setq solarized-termcolors 256
      ;; solarized-degrade t
      solarized-bold t
      solarized-underline t
      solarized-italic t
      solarized-contrast 'normal ; 'normal, 'hight, 'low
      solarized-visibility 'high ; 'normal, 'high, 'low
      ;; solarized-broken-srgb nil    ; nil, t
      )


;;; color-theme-almost-monokai

;; (color-theme-almost-monokai)

;;; monokai-theme

;; (load-file "~/.emacs.d/init/color-themes/monokai-theme.el")
;; ;; (load-file "~/.emacs.d/init/color-themes/molokai-theme.el")

;; (load-theme 'monokai t)

;;; [ Faces ]
;; italic & bold
(set-face-attribute 'italic nil
                    :slant 'italic
                    :foreground "white")
(set-face-attribute 'bold nil
                    :weight 'bold
                    :foreground "white")
(set-face-attribute 'underline nil
                    :underline "white")
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

(set-face-attribute 'show-paren-match-face nil
                    :foreground "black" :background "lawn green"
                    :weight 'normal
                    )
(set-face-attribute 'show-paren-mismatch-face nil
                    :foreground "black" :background "brown"
                    :box nil)


;;; [ prettify-symbols-mode ]

;; (setq prettify-symbols-alist '(("lambda" . 955)))

(global-prettify-symbols-mode 1)


;;; [ pretty-mode ] -- Redisplay parts of the Emacs buffer as pretty symbols.

;; https://github.com/akatov/pretty-mode

(require 'pretty-mode)

;; FIXME:
;; (add-to-list pretty-patterns '((ruby-mode ("->" . ?λ))))

;; (setq-default pretty-patterns
;;               '(let* ((lispy '(scheme emacs-lisp lisp))
;;                       (mley '(tuareg haskell sml))
;;                       (c-like '(c c++ perl sh python java ess ruby))
;;                       (Org '(org markdown))
;;                       (TeX '(tex latex))
;;                       (all (append lispy mley c-like Org TeX (list '(octave)))))
;;                  (pretty-compile-patterns
;;                   `(
;;                     (?≠ ("!=" ,@c-like scheme octave Org)
;;                         ("<>" tuareg octave)
;;                         ("~=" octave)
;;                         ("/=" haskell emacs-lisp))
;;                     (?≤ ("<=" ,@all))
;;                     (?≥ (">=" ,@all))
;;                     ;; ⇨ ⇦, ← →
;;                     (?⇦ ("<-" ,@mley ess Org))
;;                     (?⇨ ("->" ,@mley ess c c++ perl Org))
;;                     (?↑ ("\\^" tuareg))
;;                     (?⇨ ("=>" sml perl ruby haskell))
;;                     (?∅ ("nil" emacs-lisp ruby)
;;                         ("null" scheme java)
;;                         ("NULL" c c++)
;;                         ("None" python)
;;                         ("()" ,@mley))
;;                     (?… ("\\.\\.\\." scheme))
;;                     (?∀ ("List.for_all" tuareg))
;;                     (?∃ ("List.exists" tuareg))
;;                     (?∈ ("List.mem" tuareg)
;;                         ("member" ,@lispy))
;;                     (?∉ ())
;;                     (?√ ("sqrt" ,@all))
;;                     (?∑ ("sum" python))
;;                     (?α ("alpha" ,@all)
;;                         ("'a" ,@mley))
;;                     (?β ("beta" ,@all)
;;                         ("'b" ,@mley))
;;                     (?γ ("gamma" ,@all)
;;                         ("'c" ,@mley))
;;                     (?Δ ("delta" ,@all)
;;                         ("'d" ,@mley))
;;                     (?ε ("epsilon" ,@all))
;;                     (?θ ("theta" ,@all))
;;                     (?λ ("lambda" ,@all)
;;                         ;; ("case-\\(lambda\\)" scheme)
;;                         ("fn" sml)
;;                         ("fun" tuareg)
;;                         ("\\" haskell))
;;                     (?π ("pi" ,@all)
;;                         ("M_PI" c c++))
;;                     (?φ ("psi" ,@all))
;;                     (?² ("**2" python tuareg octave)
;;                         ("^2" octave haskell))
;;                     (?³ ("**3" python tuareg octave)
;;                         ("^3" octave haskell))
;;                     (?ⁿ ("**n" python tuareg octave)
;;                         ("^n" octave haskell))
;;                     ;; (?₀ ("[0]" ,@c-like)
;;                     ;;     ("(0)" octave)
;;                     ;;     (".(0)" tuareg))
;;                     ;; (?₁ ("[1]" ,@c-like)
;;                     ;;     ("(1)" octave)
;;                     ;;     (".(1)" tuareg))
;;                     ;; (?₂ ("[2]" ,@c-like)
;;                     ;;     ("(2)" octave)
;;                     ;;     (".(2)" tuareg))
;;                     ;; (?₃ ("[3]" ,@c-like)
;;                     ;;     ("(3)" octave)
;;                     ;;     (".(3)" tuareg))
;;                     ;; (?₄ ("[4]" ,@c-like)
;;                     ;;     ("(4)" octave)
;;                     ;;     (".(4)" tuareg))
;;                     (?∞ ("HUGE_VAL" c c++))
;;                     ;;    (?∙ ())
;;                     ;;    (?× ())
;;                     ;;    (?ₐ ("[a]" ,@c-like)
;;                     ;;        ("(a)" octave))
;;                     ;;    (?ₓ ("[x]" ,@c-like)
;;                     ;;        ("(x)" octave))
;;                     ;;    (?₅ ("[5]") ,@c-like)
;;                     ;;    (?₆ ("[6]") ,@c-like)
;;                     ;;    (?₇ ("[7]") ,@c-like)
;;                     ;;    (?₈ ("[8]") ,@c-like)
;;                     ;;    (?₉ ("[9]") ,@c-like)
;;                     ;;    (?⋂ "\\<intersection\\>"   (,@lispen))
;;                     ;;    (?⋃ "\\<union\\>"          (,@lispen))
;;                     (?∧ ("\\<And\\>"     emacs-lisp lisp python)
;;                         ("\\<andalso\\>" sml)
;;                         ("&&"            c c++ perl haskell))
;;                     (?∨ ("\\<or\\>"      emacs-lisp lisp)
;;                         ("\\<orelse\\>"  sml)
;;                         ("||"            c c++ perl haskell))
;;                     (?¬ ("\\<!\\>"       c c++ perl sh)
;;                         ("\\<not\\>"     lisp emacs-lisp scheme haskell sml))
;;                     ))))

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


;;; [ highlight-symbol ] -- automatic and manual symbol highlighting for Emacs

;;; Usage:
;;
;; Use highlight-symbol-at-point to toggle highlighting of the symbol at point
;; throughout the current buffer. Use highlight-symbol-mode to keep the symbol
;; at point highlighted.
;;
;; The functions highlight-symbol-next, highlight-symbol-prev,
;; highlight-symbol-next-in-defun and highlight-symbol-prev-in-defun allow for
;; cycling through the locations of any symbol at point. Use
;; highlight-symbol-nav-mode to enable key bindings (M-p and M-p) for
;; navigation. When highlight-symbol-on-navigation-p is set, highlighting is
;; triggered regardless of highlight-symbol-idle-delay.
;;
;; highlight-symbol-query-replace can be used to replace the symbol.

(require 'highlight-symbol)

;; (global-set-key [(control f3)] 'highlight-symbol-at-point)
;; (global-set-key [f3] 'highlight-symbol-next)
;; (global-set-key [(shift f3)] 'highlight-symbol-prev)
;; (global-set-key [(meta f3)] 'highlight-symbol-query-replace)

(setq highlight-symbol-idle-delay 1.3
      highlight-symbol-border-pattern '("\\_<" . "\\_>")
      )

(set-face-attribute 'highlight-symbol-face nil
                    :foreground " " :background "green yellow")

(add-hook 'prog-mode-hook 'highlight-symbol-mode)

(define-key my-search-prefix-map (kbd "h") 'highlight-symbol-at-point)


;;; [ page-break-lines-mode ] --- page breaks (^L characters) are displayed as a horizontal line of a character.

;;; In Page Break mode, page breaks (^L characters) are displayed as a horizontal line of `page-break-string-char' characters.

(require 'page-break-lines)

(global-page-break-lines-mode t)
(diminish 'page-break-lines-mode)

(setq page-break-lines-char ?─)


;;; [ Fold ]

;;; hs-minor-mode --

;; FIXME: *ERROR*: Web Mode doesn't support Hideshow Minor Mode.
;; (add-hook 'prog-mode-hook 'hs-minor-mode)


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

;; (diminish 'minimap-mode)


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
