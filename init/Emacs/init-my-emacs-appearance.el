;;; init-my-emacs-appearance.el --- my Emacs apperance init

;;; Commentary:


;;; Code:


(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;; [ Title ]

(setq frame-title-format "Emacs λ Hacking")

;; (setq frame-title-format "Emacs λ %b")

;; (setq frame-title-format
;;       '("" invocation-name ": "
;;         (:eval
;;          (if (buffer-file-name)
;;              (abbreviate-file-name (buffer-file-name))
;;            "%b"))))

;;; [ tabbar ] -- show tabbar for buffers at headline.

;; (use-package tabbar
;;   :ensure t
;;   :config
;;   (setq tabbar-use-images nil)
;;   ;; (tabbar-mode 1)
;;   (use-package tabbar-ruler
;;     :ensure t
;;     :bind ("C-t" . tabbar-ruler-move)
;;     :config
;;     (setq tabbar-ruler-global-tabbar t)    ; get tabbar
;;     (setq tabbar-ruler-global-ruler t)     ; get global ruler
;;     (setq tabbar-ruler-popup-menu t)       ; get popup menu.
;;     (setq tabbar-ruler-popup-toolbar t)    ; get popup toolbar
;;     (setq tabbar-ruler-popup-scrollbar t)  ; show scroll-bar on mouse-move
;;
;;     (tabbar-ruler-group-by-projectile-project) ; group by projectile project.
;;     )
;;   )

;;; [ border ]

;; frame internal border width
(set-frame-parameter (selected-frame) 'internal-border-width 5)


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
;; (fringe-mode nil)


;;; [ echo area ]

(setq-default echo-keystrokes 0.1) ; faster echo key strokes


;;; [ Widget ]


;;; [ line space(spacing) / line height ]
;; - (info "(elisp) Line Height")
;; - (info "(elisp) Layout Parameters")
;; The total height of each display line consists of the height of the
;; contents of the line, plus optional additional vertical line spacing
;; above or below the display line.

;; additional space to put between lines.
(setq-default line-spacing 0.1)         ; 0.1, 1, 0, nil.


;;; [ line number ]

;; (use-package linum
;;   :defer t
;;   :config
;;   ;; Linum: separating line numbers from text
;;   ;; (setq linum-format 'dynamic)
;;   ;; (setq linum-format "%d ") ; 'dynamic
;;   (setq linum-format "%4d \u2502") ; a solid line separator
;;   ;; combine 'dynamic result with \u2502
;;   ;; (setq linum-format '(combine 'dynamic "\u2502"))
;;
;;   ;; set line number face
;;   (set-face-attribute 'linum nil
;;                       :foreground "#444444"
;;                       )
;;
;;   ;; (global-linum-mode 1)
;;   ;; but show line numbers in source code files
;;   (add-hook 'prog-mode-hook 'linum-mode)
;;
;;   ;; Turn off linum-mode when file is too big.
;;   (add-hook 'prog-mode-hook
;;             (lambda ()
;;               ;; turn off `linum-mode' when there are more than 5000 lines
;;               ;; use `wc -c file' for performance reason
;;               (if (and (executable-find "wc")
;;                        (> (string-to-number
;;                            (shell-command-to-string
;;                             (format "wc -c %s" (buffer-file-name))))
;;                           (* 5000 80)))
;;                   (linum-mode -1))))
;;   )


;;; [ nlinum ] -- show line numbers in margin.

;; (use-package nlinum
;;   :ensure t
;;   :defer t
;;   :config
;;   (setq nlinum-format "%d ")
;;   ;; TODO: (nlinum-mode 1)
;;   )

;;; [ highlight ] -- Highlighting commands.

;; (use-package highlight
;;   :defer t
;;   :config
;;   ;; (set-face-attribute 'highlight nil
;;   ;;                     :inherit nil
;;   ;;                     :foreground nil
;;   ;;                     :background (color-darken-name
;;   ;;                                  (face-background 'default) 3)
;;   ;;                     )
;;   )


;;; [ current line & column ]

;; highlight current line
(use-package hl-line
  :init
  (global-hl-line-mode 1)
  )



;;; [ point & cursor ]

(setq-default mouse-avoidance-mode 'animate ; auto move mouse away when cursor is at mouse position
              cursor-in-echo-area nil
              mouse-yank-at-point t
              blink-cursor-blinks 10
              )

;;; horizontal bar
(setq-default cursor-type t ; '(hbar . 2)
              cursor-in-non-selected-windows nil)
;; (set-cursor-color "cyan")
(set-cursor-color "DeepSkyBlue")

;;; hollow
;; (setq-default cursor-type 'hollow
;;               cursor-in-non-selected-windows nil)
;; (set-cursor-color "green")

;;; adaptive cursor width
;; make cursor the width of the character it is under
;; i.e. full width of a TAB
(setq x-stretch-cursor t)

(blink-cursor-mode 1)

;;; [ beacon ] -- highlight the cursor whenever the window scrolls.

;; (use-package beacon
;;   :ensure t
;;   :config
;;   (beacon-mode 1)
;;   (setq beacon-color "green yellow")
;;   )


;;; [ Selection ]

(setq transient-mark-mode t)


;;; [ wrap & fill ]

;; default column length (80)
(setq-default fill-column 80)

;; disable soft wrap lines for windows which has smaller width than 80.
(global-visual-line-mode -1) ; soft wrap lines at word boundary

;;; toggle fill/un-fill
(defun my/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'my/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(global-set-key [remap fill-paragraph] #'my/fill-or-unfill)


;;; [ fci ] -- Fill Column Indicator

;; (with-eval-after-load 'fill-column-indicator
;;   (setq fci-rule-width 10)
;;   (setq fci-rule-character ?❚)
;;   ;; (setq fci-rule-character-color "#999999")
;;   (setq fci-dash-pattern 1.00))


;;; [ auto-fill-mode ] -- auto fill paragraphs like hitting [M-q].

;;; global
;; (auto-fill-mode t)
;;
;;; auto fill comments but not code in programming modes:
(add-hook 'prog-mode-hook
          '(lambda ()
             (setq-local comment-auto-fill-only-comments t)
             ))
;;
;;; enable only for text writing modes.
(toggle-text-mode-auto-fill)
(dolist (hook
         '(text-mode-hook
           org-mode-hook
           markdown-mode-hook))
  (add-hook hook 'turn-on-auto-fill))

;;; [ page (^L) ]

;; - <C-x [/]> :: navigate.
;; "^\014",
;; (setq page-delimiter
;;       (rx bol ";;;" (not (any "#")) (* not-newline) "\n"
;;           (* (* blank) (opt ";" (* not-newline)) "\n")))
;; Expanded regexp:
;; "^;;;[^#].*\n\\(?:[[:blank:]]*\\(?:;.*\\)?\n\\)*"
;;
;; The regexp above is a bit special. We’re setting the page delimiter to be a
;; ;;; at the start of a line, plus any number of empty lines or comment lines
;; that follow it (that # part is to exclude ;;;###autoload cookies).


;;; Disable GUI dialog boxes

(setq use-dialog-box nil) ; use mini-buffer for everything' else..


;;; trailing whitespace

;;; [ page-break-lines ] -- visual horizontal line for page break.

(use-package page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode t))

;;; [ all-the-icons ] -- A utility package to collect various Icon Fonts and propertize them within Emacs.

(use-package all-the-icons
  :ensure t
  :defer t)



(provide 'init-my-emacs-appearance)

;;; init-my-emacs-appearance.el ends here
