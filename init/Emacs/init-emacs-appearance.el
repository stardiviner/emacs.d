;;; init-emacs-appearance.el --- my Emacs apperance init

;;; Commentary:


;;; Code:


(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;; [ Transparent ]

(defun my:set-transparency-alpha (alpha)
  "Loop through transparency `ALPHA' settings."
  (interactive (list
                (completing-read "Transparency Alpha: "
                                 '("unset" "100" "95" "90" "80" "70" "60"))))
  (if (string= alpha "unset")
      (progn
        (set-frame-parameter (selected-frame) 'alpha (list 100 100))
        (add-to-list 'default-frame-alist (cons 'alpha (list 100 100))))
    (let* ((active-alpha (string-to-number alpha))
           (inactive-alpha (- active-alpha 20)))
      (set-frame-parameter (selected-frame) 'alpha (list active-alpha inactive-alpha))
      (add-to-list 'default-frame-alist (cons 'alpha (list active-alpha inactive-alpha)))
      )))

(my:set-transparency-alpha "95")

;;; [ Title ]

;; (setq frame-title-format "Emacs λ Hacking")

;; (setq frame-title-format "Emacs λ %b")

;; (setq frame-title-format
;;       '("" invocation-name ": "
;;         (:eval
;;          (if (buffer-file-name)
;;              (abbreviate-file-name (buffer-file-name))
;;            "%b"))))

;;; [ border & margin ]

(setq-default left-margin-width 0
              right-margin-width 0)
;; frame internal border width
(set-frame-parameter nil 'internal-border-width 5)
;; (set-window-buffer nil (current-buffer))

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


;;; [ Widget ]

;;; [ line ]

;;; [ line space(spacing) / line height ]
;; - (info "(elisp) Line Height")
;; - (info "(elisp) Layout Parameters")
;; The total height of each display line consists of the height of the
;; contents of the line, plus optional additional vertical line spacing
;; above or below the display line.

;; additional space to put between lines.
(setq-default line-spacing 0)         ; 0.1, 1, 0, nil.

;;; [ line numbers ] -- Emacs native line number mode.

;; (setq display-line-numbers-width-start t) ; right-align
;; (global-display-line-numbers-mode 1)

;;; [ current line & column ]

;; highlight current line
(global-hl-line-mode 1)

;;; [ point & cursor ]

(setq-default mouse-avoidance-mode 'animate ; auto move mouse away when cursor is at mouse position
              cursor-in-echo-area nil
              mouse-yank-at-point t)

;;; horizontal bar
(setq-default cursor-type t ; '(hbar . 2)
              cursor-in-non-selected-windows nil)
;; (set-cursor-color "cyan")
(set-cursor-color "deep pink")

;;; hollow
;; (setq-default cursor-type 'hollow
;;               cursor-in-non-selected-windows nil)
;; (set-cursor-color "green")

;;; adaptive cursor width
;; make cursor the width of the character it is under
;; i.e. full width of a TAB
(setq x-stretch-cursor t)

;; (setq blink-cursor-blinks 10)
;; (blink-cursor-mode 1)
(setq blink-cursor-mode nil) ; disable `blink-cursor-mode'.

;;; [ truncate continuous line & word wrap ]

;; (setq-default truncate-lines t
;;               word-wrap t)

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

(use-package aggressive-fill-paragraph
  :ensure t
  :defer t
  :init (add-hook 'text-mode-hook #'aggressive-fill-paragraph-mode)
  :config (add-to-list 'afp-fill-keys ?。) ; support for Chinese sentence end.
  (add-to-list 'afp-fill-comments-only-mode-list 'emacs-lisp-mode)
  (add-to-list 'afp-fill-comments-only-mode-list 'lisp-mode)
  (add-to-list 'afp-fill-comments-only-mode-list 'clojure-mode))

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

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

;; (require 'whitespace)
;; (setq whitespace-line-column 80) ; limit line length
;; (setq whitespace-style '(face lines-tail))
;; (add-hook 'prog-mode-hook 'whitespace-mode)
;; (global-whitespace-mode +1)


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
  )



(provide 'init-emacs-appearance)

;;; init-emacs-appearance.el ends here
