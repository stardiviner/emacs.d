;;; solarized-dark-theme.el --- Solarized color-theme
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(require 'color)
(require 'color-theme)


;; name     sRGB      Gen RGB   256       16              8
'((base03  "#002b36" "#042028" "#1c1c1c" "brightblack"   "black")
  (base02  "#073642" "#0a2832" "#262626" "black"         "black")
  (base01  "#586e75" "#465a61" "#585858" "brightgreen"   "green")
  (base00  "#657b83" "#52676f" "#626262" "brightyellow"  "yellow")
  (base0   "#839496" "#708183" "#808080" "brightblue"    "blue")
  (base1   "#93a1a1" "#81908f" "#8a8a8a" "brightcyan"    "cyan")
  (base2   "#eee8d5" "#e9e2cb" "#e4e4e4" "white"         "white")
  (base3   "#fdf6e3" "#fcf4dc" "#ffffd7" "brightwhite"   "white")
  (yellow  "#b58900" "#a57705" "#af8700" "yellow"        "yellow")
  (orange  "#cb4b16" "#bd3612" "#d75f00" "brightred"     "red")
  (red     "#dc322f" "#c60007" "#d70000" "red"           "red")
  (magenta "#d33682" "#c61b6e" "#af005f" "magenta"       "magenta")
  (violet  "#6c71c4" "#5859b7" "#5f5faf" "brightmagenta" "magenta")
  (blue    "#268bd2" "#2075c7" "#0087ff" "blue"          "blue")
  (cyan    "#2aa198" "#259185" "#00afaf" "cyan"          "cyan")
  (green   "#859900" "#728a05" "#5f8700" "green"         "green"))


(defvar solarized-dark-colors
  '((base03  "#002b36")
    (base02  "#073642")
    (base01  "#586e75")
    (base00  "#657b83")
    (base0   "#839496")
    (base1   "#93a1a1")
    (base2   "#eee8d5")
    (base3   "#fdf6e3")
    (yellow  "#b58900")
    (orange  "#cb4b16")
    (red     "#dc322f")
    (magenta "#d33682")
    (violet  "#6c71c4")
    (blue    "#268bd2")
    (cyan    "#2aa198")
    (green   "#859900"))
  )

(defun color-theme-solarized-dark ()
  "A basic solarized dark color-theme."
  (interactive)
  (color-theme-install
   '(color-theme-solarized-dark
     ((background-color . "#002b36")
      (background-mode . dark)
      (foreground-color . "#839496")
      (border-color . "black")
      (cursor-color . "cyan"))
     ;; font-lock
     (font-lock-builtin-face ((t (:foreground "#859900")))) ; Built-in
     (font-lock-comment-delimiter-face ((t (:foreground "#586e75")))) ; Comment
     (font-lock-comment-face ((t (:foreground "#586e75")))) ; Comment
     (font-lock-constant-face ((t (:foreground "#2aa198")))) ; Constant
     (font-lock-doc-face ((t (:foreground "#586e75")))) ; Comment
     (font-lock-function-name-face ((t (:foreground "#268bd2")))) ; Identifier
     (font-lock-keyword-face ((t (:foreground "#859900")))) ; Statement
     (font-lock-negation-char-face ((t (:foreground "#dc322f"))))
     (font-lock-preprocessor-face ((t (:foreground "#cb4b16")))) ; PreProc
     (font-lock-regexp-grouping-backslash ((t (:foreground "#b58900"))))
     (font-lock-regexp-grouping-construct ((t (:foreground "#b58900"))))
     (font-lock-string-face ((t (:foreground "#2aa198")))); Constant
     (font-lock-type-face ((t (:foreground "#b58900")))) ; Type
     (font-lock-variable-name-face ((t (:foreground "#268bd2")))) ; Identifier
     (font-lock-warning-face ((t (:foreground "#dc322f")))) ; Error
     ;; diff
     (diff-refine-added ((t (:background "dark green"))))
     (diff-refine-removed ((t (:background "dark red"))))
     (diff-refine-change ((t (:background "white"))))
     )))


(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-solarized-dark)))

(add-to-list 'color-themes '(color-theme-solarized-dark "color-theme-solarized-dark" "solarized-dark"))

(deftheme solarized-dark
  "A simple color-theme with basic solarized colors definition."
  )


(provide 'solarized-dark-theme)

;;; solarized-dark-theme.el ends here
