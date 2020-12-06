;;; init-emacs-appearance.el --- my Emacs apperance init

;;; Commentary:


;;; Code:


(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;; [ Transparent ]

;;; [ transwin ] -- Make window/frame transparent.

;; (use-package transwin
;;   :ensure t
;;   :init (transwin-ask-set-transparency 95))

;;; [ Title ]

;;; NOTE set title for KDE KWin manager matching PDF viewer frame.
(setq frame-title-format
      '("" invocation-name " λ "
        (:eval
         (cond
          ((eq major-mode 'eaf-mode)
           (format "EAF:%s" eaf--buffer-app-name))
          ((buffer-file-name)
           (or (file-name-nondirectory (buffer-file-name)) "%b"))))))

;;; [ scroll bar ]

;; (scroll-bar-mode 1)

;;; [ Emacs window border ]

(setq-default indicate-buffer-boundaries 'left)

;;; [[info:elisp#Layout Parameters][info:elisp#Layout Parameters]]

(defun my/set-frame-border-width ()
  "Set frame default fringe, margin, border width."
  (let* ((hidpi-p (screen-hidpi-p))
         (fringe-width (if hidpi-p 8 4))
         (margin-width (if hidpi-p 1 0))
         (border-width (if hidpi-p 2 1)))
    (set-fringe-style (cons fringe-width fringe-width))
    (setq-default left-fringe-width fringe-width
                  right-fringe-width fringe-width)
    (setq-default left-margin-width margin-width
                  right-margin-width margin-width)
    (set-frame-parameter nil 'internal-border-width border-width)
    (set-frame-parameter nil 'vertical-scroll-bars nil)
    (set-frame-parameter nil 'horizontal-scroll-bars nil)
    (set-frame-parameter nil 'scroll-bars-width nil)
    (set-frame-parameter nil 'left-fringe nil)
    (set-frame-parameter nil 'right-fringe nil)
    (set-frame-parameter nil 'right-divider-width 0)
    (set-frame-parameter nil 'bottom-divider-width 0)))

(my/set-frame-border-width)

;;; `fringe-indicator-alist'
(setq fringe-indicator-alist
      '((truncation left-arrow right-arrow)
        (continuation left-curly-arrow right-curly-arrow)
        (overlay-arrow . right-triangle)
        (up . up-arrow)
        (down . down-arrow)
        (top top-left-angle top-right-angle)
        (bottom bottom-left-angle bottom-right-angle top-right-angle top-left-angle)
        (top-bottom left-bracket right-bracket top-right-angle top-left-angle)
        (empty-line . empty-line)
        (unknown . question-mark)))

;;; HiDPI fringe bitmaps
(when (screen-hidpi-p)
  (define-fringe-bitmap 'left-curly-arrow
    (vector #b0011111110000000
            #b0011111110000000
            #b0011111110000000
            #b0011100000000000
            #b0011100000000000
            #b0011100000000000
            #b0011100000000000
            #b0011100001000000
            #b0011100001100000
            #b0011100001110000
            #b0011111111111000
            #b0011111111111100
            #b0011111111111000
            #b0000000001110000
            #b0000000001100000
            #b0000000001000000
            )
    16 16)

  (define-fringe-bitmap 'right-curly-arrow
    (vector #b0000000111111100
            #b0000000111111100
            #b0000000111111100
            #b0000000000011100
            #b0000000000011100
            #b0000000000011100
            #b0000000000011100
            #b0000001000011100
            #b0000011000011100
            #b0000111000011100
            #b0001111111111100
            #b0011111111111100
            #b0001111111111100
            #b0000111000000000
            #b0000011000000000
            #b0000001000000000
            )
    16 16)

  (define-fringe-bitmap 'left-arrow
    (vector #b0000000001000000
            #b0000000011000000
            #b0000000111000000
            #b0000001111000000
            #b0000011110000000
            #b0000111100000000
            #b0001111111111100
            #b0011111111111100
            #b0011111111111100
            #b0001111111111100
            #b0000111100000000
            #b0000011110000000
            #b0000001111000000
            #b0000000111000000
            #b0000000011000000
            #b0000000001000000
            )
    16 16)

  (define-fringe-bitmap 'right-arrow
    (vector #b0000001000000000
            #b0000001100000000
            #b0000001110000000
            #b0000001111000000
            #b0000000111100000
            #b0000000011110000
            #b0011111111111000
            #b0011111111111100
            #b0011111111111100
            #b0011111111111000
            #b0000000011110000
            #b0000000111100000
            #b0000001111000000
            #b0000001110000000
            #b0000001100000000
            #b0000001000000000
            )
    16 16)

  (define-fringe-bitmap 'right-triangle
    (vector #b0000000000000000
            #b0000000000000000
            #b0011000000000000
            #b0011110000000000
            #b0011111100000000
            #b0011111111000000
            #b0011111111110000
            #b0011111111111100
            #b0011111111111100
            #b0011111111110000
            #b0011111111000000
            #b0011111100000000
            #b0011110000000000
            #b0011000000000000
            #b0000000000000000
            #b0000000000000000
            )
    16 16)
  
  ;; Bitmap for breakpoint in fringe
  (define-fringe-bitmap 'breakpoint
    ;; "\x3c\x7e\xff\xff\xff\xff\x7e\x3c"
    (vector #b0000000000000000
            #b0000000000000000
            #b0011000000000000
            #b0011110000000000
            #b0011111100000000
            #b0011111111000000
            #b0011111111110000
            #b0011111111111100
            #b0011111111111100
            #b0011111111110000
            #b0011111111000000
            #b0011111100000000
            #b0011110000000000
            #b0011000000000000
            #b0000000000000000
            #b0000000000000000
            )
    16 16)
  ;; Bitmap for gud-overlay-arrow in fringe
  (define-fringe-bitmap 'hollow-right-triangle
    ;; "\xe0\x90\x88\x84\x84\x88\x90\xe0"
    (vector #b0000000000000000
            #b0000000000000000
            #b0011000000000000
            #b0011110000000000
            #b0011111100000000
            #b0011111111000000
            #b0011111111110000
            #b0011111111111100
            #b0011111111111100
            #b0011111111110000
            #b0011111111000000
            #b0011111100000000
            #b0011110000000000
            #b0011000000000000
            #b0000000000000000
            #b0000000000000000
            )
    16 16)

  (with-eval-after-load 'gdb
   (setq gdb-buffer-fringe-width (* gdb-buffer-fringe-width 2)))

  ;; for `realgud'
  (define-fringe-bitmap 'arrow-bitmap
    ;; "\xc0\xf0\xf8\xfc\xfc\xf8\xf0\xc0"
    (vector #b0000000000000000
            #b0000000000000000
            #b0011000000000000
            #b0011110000000000
            #b0011111100000000
            #b0011111111000000
            #b0011111111110000
            #b0011111111111100
            #b0011111111111100
            #b0011111111110000
            #b0011111111000000
            #b0011111100000000
            #b0011110000000000
            #b0011000000000000
            #b0000000000000000
            #b0000000000000000
            )
    16 16)

  (define-fringe-bitmap 'realgud-bp-filled
    ;; "\x3c\x7e\xff\xff\xff\xff\x7e\x3c"
    (vector #b0000001000000000
            #b0000001100000000
            #b0000001110000000
            #b0000001111000000
            #b0000000111100000
            #b0000000011110000
            #b0011111111111000
            #b0011111111111100
            #b0011111111111100
            #b0011111111111000
            #b0000000011110000
            #b0000000111100000
            #b0000001111000000
            #b0000001110000000
            #b0000001100000000
            #b0000001000000000
            )
    16 16)
  (define-fringe-bitmap 'realgud-bp-hollow
    ;; "\x3c\x42\x81\x81\x81\x81\x42\x3c"
    (vector #b0000001000000000
            #b0000001100000000
            #b0000001110000000
            #b0000001111000000
            #b0000000111100000
            #b0000000011110000
            #b0011111111111000
            #b0011111111111100
            #b0011111111111100
            #b0011111111111000
            #b0000000011110000
            #b0000000111100000
            #b0000001111000000
            #b0000001110000000
            #b0000001100000000
            #b0000001000000000
            )
    16 16)
  )

;;; [ echo area ]


;;; [ Widget ]

;;; [ line ]

;;; [ line space(spacing) / line height ]
;; - (info "(elisp) Line Height")
;; - (info "(elisp) Layout Parameters")
;; The total height of each display line consists of the height of the
;; contents of the line, plus optional additional vertical line spacing
;; above or below the display line.

;; additional space to put between lines.
;; (setq-default line-spacing 0.1)         ; nil, 0, 0.1, 1,

;;; [ line numbers ] -- Emacs native line number mode.

;; (setq display-line-numbers-width-start t)
;; (global-display-line-numbers-mode 1)

;;; [ current line & column ]

;; highlight current line
(use-package hl-line
  :ensure nil
  :defer t
  :hook (after-init . global-hl-line-mode))

;;; [ point & cursor ]

;; auto move mouse away when cursor is at mouse position
(setq-default mouse-avoidance-mode 'animate)

(setq-default cursor-type t ; '(hbar . 2), 'hollow,
              cursor-in-non-selected-windows nil)
;; (set-cursor-color "cyan") ; "deep pink"

;; (blink-cursor-mode 1)

;;; NOTE: ‘blink-cursor-mode’ caused in-buffer picture blink.
(blink-cursor-mode -1)

;;; [ truncate continuous line & word wrap ]

;;; `toggle-truncate-lines'
;; (setq-default truncate-lines t)
(setq truncate-string-ellipsis "…")
;;
;;; `toggle-word-wrap'
;; (setq-default word-wrap t)

(define-fringe-bitmap 'left-curly-arrow
  [#b00011000
   #b00011000
   #b00000000
   #b00011000
   #b00011000
   #b00000000
   #b00011000
   #b00011000])
(define-fringe-bitmap 'right-curly-arrow
  [#b00011000
   #b00011000
   #b00000000
   #b00011000
   #b00011000
   #b00000000
   #b00011000
   #b00011000])

;;; [ auto fill ] -- auto fill (break line) paragraphs like hitting [M-q].

(setq-default fill-column 80
              comment-fill-column 80)
;;; auto fill comments but not code in programming modes:
(add-hook 'prog-mode-hook
          #'(lambda () (setq-local comment-auto-fill-only-comments t)))

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; (global-display-fill-column-indicator-mode 1)

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

(use-package whitespace
  :hook ((markdown-mode conf-mode) . whitespace-mode)
  :config
  (setq whitespace-line-column 80) ; highlight beyond limit line length
  
  ;; Don't use different background for tabs.
  (face-spec-set 'whitespace-tab
                 '((t :background unspecified)))
  ;; Only use background and underline for long lines, so we can still have
  ;; syntax highlight.

  ;; For some reason use face-defface-spec as spec-type doesn't work.  My guess
  ;; is it's due to the variables with the same name as the faces in
  ;; whitespace.el.  Anyway, we have to manually set some attribute to
  ;; unspecified here.
  (face-spec-set 'whitespace-line
                 '((((background light))
                    :background "#d8d8d8" :foreground unspecified
                    :underline t :weight unspecified)
                   (t
                    :background "#404040" :foreground unspecified
                    :underline t :weight unspecified)))

  ;; Use softer visual cue for space before tabs.
  (face-spec-set 'whitespace-space-before-tab
                 '((((background light))
                    :background "#d8d8d8" :foreground "#de4da1")
                   (t
                    :inherit warning
                    :background "#404040" :foreground "#ee6aa7")))

  (setq whitespace-line-column nil
        whitespace-style '(face             ; visualize things below:
                           empty            ; empty lines at beginning/end of buffer
                           lines-tail       ; lines go beyond `fill-column'
                           space-before-tab ; spaces before tab
                           trailing         ; trailing blanks
                           tabs             ; tabs (show by face)
                           tab-mark         ; tabs (show by symbol)
                           )))

;;; [ all-the-icons ] -- A utility package to collect various Icon Fonts and propertize them within Emacs.

(use-package all-the-icons
  :ensure t
  :defer t
  :config
  (add-to-list 'all-the-icons-mode-icon-alist
               '(circe-channel-mode all-the-icons-faicon "commenting-o" :height 1.0 :v-adjust 0.0 :face all-the-icons-cyan))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ag-mode all-the-icons-faicon "search" :height 1.0 :v-adjust 0.0 :face all-the-icons-cyan))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(rg-mode all-the-icons-faicon "search" :height 1.0 :v-adjust 0.0 :face all-the-icons-cyan))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ripgrep-search-mode all-the-icons-faicon "search" :height 1.0 :v-adjust 0.0 :face all-the-icons-cyan))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(sql-interactive-mode all-the-icons-faicon "database" 1.0 :v-adjust 0.05 :face all-the-icons-lgreen))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(arduino-mode all-the-icons-fileicon "arduino" 1.0 :v-adjust 0.00 :face all-the-icons-blue-alt))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(lisp-mode all-the-icons-fileicon "clisp" :height 1.0 :v-adjust 0.0 :face all-the-icons-blue-alt))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(inferior-lisp-mode all-the-icons-fileicon "lisp" :height 1.0 :v-adjust 0.0 :face all-the-icons-cyan-alt))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(inf-clojure-mode all-the-icons-fileicon "clj" :height 1.0 :v-adjust 0.0 :face all-the-icons-maroon))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(py-python-shell-mode all-the-icons-alltheicon "python" :height 1.0 :v-adjust 0.0 :face all-the-icons-blue-alt))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(js-comint-mode all-the-icons-alltheicon "javascript" :height 1.0 :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(indium-repl-mode all-the-icons-alltheicon "javascript" :height 1.0 :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(skewer-repl-mode all-the-icons-alltheicon "javascript" :height 1.0 :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(inferior-ess-mode all-the-icons-fileicon "R" :height 1.0 :v-adjust 0.0 :face all-the-icons-blue-alt))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(julia-mode all-the-icons-fileicon "julia" :height 1.0 :v-adjust 0.0 :face all-the-icons-blue-alt))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(lua-mode all-the-icons-fileicon "lua" :height 1.0 :v-adjust 0.0 :face all-the-icons-blue-alt))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(package-menu-mode all-the-icons-octicon "package" :height 1.0 :v-adjust 0.0 :face all-the-icons-cyan-alt))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(doc-view-mode all-the-icons-faicon "file-pdf-o" :height 1.0 :v-adjust 0.0 :face all-the-icons-dgreen))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(pdf-view-mode all-the-icons-faicon "file-pdf-o" :height 1.0 :v-adjust 0.0 :face all-the-icons-dgreen))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(nov-mode all-the-icons-faicon "file-text-o" :height 1.0 :v-adjust 0.0 :face all-the-icons-dgreen))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ereader-mode all-the-icons-faicon "file-text-o" :height 1.0 :v-adjust 0.0 :face all-the-icons-dgreen))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(Info-mode all-the-icons-faicon "file-o" :height 1.0 :v-adjust 0.0 :face all-the-icons-cyan-alt))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(Man-mode all-the-icons-faicon "file-o" :height 1.0 :v-adjust 0.0 :face all-the-icons-cyan-alt))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(diff-mode all-the-icons-fileicon "diff" :height 1.0 :v-adjust 0.0 :face all-the-icons-maroon))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(magit-revision-mode all-the-icons-octicon "git-commit" :height 1.0 :v-adjust 0.0 :face all-the-icons-maroon))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(debugger-mode all-the-icons-faicon "cogs" :height 1.0 :v-adjust 0.0 :face all-the-icons-maroon))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(latex-mode all-the-icons-fileicon "tex" :height 1.0 :v-adjust 0.0 :face all-the-icons-cyan))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(coq-mode all-the-icons-fileicon "coq" :height 1.0 :v-adjust 0.0 :face all-the-icons-maroon))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(tar-mode all-the-icons-faicon "file-archive-o" :height 1.0 :v-adjust 0.0 :face all-the-icons-maroon))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(systemd-mode all-the-icons-faicon "linux" :height 1.0 :v-adjust 0.0 :face all-the-icons-lred))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(logview-mode all-the-icons-faicon "floppy-o" :height 1.0 :v-adjust 0.0 :face all-the-icons-lyellow))
  ;; (add-to-list 'all-the-icons-mode-icon-alist
  ;;              '(plantuml-mode all-the-icons-faicon "" :height 1.0 :v-adjust 0.0))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(org-tree-slide-mode all-the-icons-faicon "file-powerpoint-o" :height 1.0 :v-adjust 0.0 :face all-the-icons-lyellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(octave-mode all-the-icons-fileicon "octave" :height 1.0 :v-adjust 0.0 :face all-the-icons-lblue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(inferior-octave-mode all-the-icons-fileicon "octave" :height 1.0 :v-adjust 0.0 :face all-the-icons-lblue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(mu4e-main-mode all-the-icons-material "email" :height 1.0 :v-adjust -0.5 :face all-the-icons-lyellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(mu4e-headers-mode all-the-icons-material "email" :height 1.0 :v-adjust -0.5 :face all-the-icons-lyellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(mu4e-view-mode all-the-icons-material "email" :height 1.0 :v-adjust -0.5 :face all-the-icons-lyellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(mu4e-compose-mode all-the-icons-material "email" :height 1.0 :v-adjust -0.5 :face all-the-icons-lyellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(slack-message-buffer-mode all-the-icons-faicon "slack" :v-adjust 0.0 :face all-the-icons-purple))

  ;;=============================================================================
  (add-to-list 'all-the-icons-icon-alist '("\\.webm" all-the-icons-faicon "file-video-o" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist '("\\.opus" all-the-icons-faicon "file-audio-o" :v-adjust 0.0 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist '("\\.pdf" all-the-icons-faicon "file-pdf-o" :v-adjust 0.0 :face all-the-icons-dred))
  (add-to-list 'all-the-icons-icon-alist '("\\.djvu" all-the-icons-faicon "file-pdf-o" :v-adjust 0.0 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist '("\\.epub" all-the-icons-faicon "file-text" :v-adjust 0.0 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist '("\\.mobi" all-the-icons-faicon "file-text" :v-adjust 0.0 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist '("\\.azw3" all-the-icons-faicon "file-text" :v-adjust 0.0 :face all-the-icons-blue))
  )

;;; [ pulse ] -- Pulsing Overlays.

(use-package pulse
  :defer t
  :commands (pulse-momentary-highlight-one-line
             pulse-momentary-highlight-region
             pulse-momentary-highlight-overlay)
  :config
  (setq pulse-command-advice-flag t)
  (add-hook 'org-babel-pre-tangle-hook #'pulse-line-hook-function)
  (add-hook 'org-babel-after-execute-hook #'pulse-line-hook-function))

;;; [ beacon ] -- Highlight the cursor whenever the window scrolls.

;; (use-package beacon
;;   :ensure t
;;   :defer t
;;   :init
;;   (setq beacon-blink-when-focused t
;;         beacon-color "light sky blue")
;;   (beacon-mode 1))



(provide 'init-emacs-appearance)

;;; init-emacs-appearance.el ends here
