;;; init-my-emacs-highlight.el --- init for highlight
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

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
;; - [C-c s h] :: prefix
;; - [M-s h] :: highlight with isearch.
;; - `highlight-symbol-query-replace' can be used to replace the symbol.

(require 'highlight-symbol)

(setq highlight-symbol-idle-delay 3
      highlight-symbol-border-pattern '("\\_<" . "\\_>")
      highlight-symbol-colors '("yellow" "DeepPink" "cyan"
                                "MediumPurple1" "SpringGreen1" "DarkOrange" "HotPink1"
                                "RoyalBlue1" "OliveDrab")
      highlight-symbol-foreground-color nil ; nil: keep original color.
      ;; highlight-symbol-border-pattern '("\\_<" . "\\_>")
      )

(set-face-attribute 'highlight-symbol-face nil
                    :foreground nil :background "midnight blue"
                    :slant 'italic)

;; (add-hook 'prog-mode-hook 'highlight-symbol-mode)

;; setting up keybindings
(unless (boundp 'my-highlight-symbol-prefix)
  (define-prefix-command 'my-highlight-symbol-prefix))
(define-key my-search-prefix (kbd "h") 'my-highlight-symbol-prefix)

(define-key my-highlight-symbol-prefix (kbd "h") 'highlight-symbol-at-point)
(define-key my-highlight-symbol-prefix (kbd "n") 'highlight-symbol-next)
(define-key my-highlight-symbol-prefix (kbd "p") 'highlight-symbol-prev)
(define-key my-highlight-symbol-prefix (kbd "l") 'highlight-symbol-list-all)
(define-key my-highlight-symbol-prefix (kbd "o") 'highlight-symbol-occur)
(define-key my-highlight-symbol-prefix (kbd "C") 'highlight-symbol-count)
(define-key my-highlight-symbol-prefix (kbd "r") 'highlight-symbol-query-replace)
(define-key my-highlight-symbol-prefix (kbd "U") 'highlight-symbol-remove-all)
(define-key my-highlight-symbol-prefix (kbd "u") 'highlight-symbol-at-point)
(define-key my-highlight-symbol-prefix (kbd "m") 'highlight-symbol-nav-mode)


;;; [ highlight-thing ] -- global minor mode to highlight the thing under point.

(use-package highlight-thing
  :config
  ;; (setq highlight-thing-what-thing 'word)
  (setq highlight-thing-delay-seconds 1.5)
  (setq highlight-thing-limit-to-defun t)
  (set-face-attribute 'highlight-thing nil
                      :foreground "#00bfff"
                      :background (color-darken-name (face-background 'default) 3)
                      )
  ;; (global-highlight-thing-mode)
  (add-hook 'prog-mode-hook 'highlight-thing-mode)
  )


;;; highlight-quoted

;; (load-file (expand-file-name "init/extensions/highlight-quoted.el" user-emacs-directory))

;; (add-hook 'prog-mode-hook 'highlight-quoted-mode)

;; ;; (setq highlight-quoted-highlight-symbols t)

;; (eval-after-load 'highlight-quoted
;;   (progn
;;     (set-face-attribute 'highlight-quoted-quote nil
;;                         :inherit 'font-lock-keyword-face)
;;     (set-face-attribute 'highlight-quoted-symbol nil
;;                         :inherit 'font-lock-constant-face))
;;   )


;;; highlight-numbers

;; (load-file (expand-file-name "init/extensions/highlight-numbers.el" user-emacs-directory))
;;
;; (add-hook 'prog-mode-hook 'highlight-numbers-mode)
;;
;; ;; (setq highlight-numbers-modelist)
;;
;; (eval-after-load 'highlight-numbers
;;   (set-face-attribute 'highlight-numbers-number nil
;;                       :inherit 'font-lock-constant-face))


;;; [ highlight-escape-sequences ]

;; (require 'highlight-escape-sequences)

;; NOTE: add the following line to the theme.
;; (put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)


;;; [ highlight-stages ]

;; (require 'highlight-stages)
;;
;; (dolist (hook '(lisp-mode-hook
;;                 lisp-interaction-mode-hook
;;                 emacs-lisp-mode-hook
;;                 scheme-mode-hook
;;                 clojure-mode-hook
;;                 cider-repl-mode-hook
;;                 ))
;;   (add-hook hook 'highlight-stages-mode))
;;
;; ;; (highlight-stages-global-mode 1)
;;
;; (set-face-attribute 'highlight-stages-negative-level-face nil
;;                     :background "#003745")
;; (set-face-attribute 'highlight-stages-level-1-face nil
;;                     :background "#001e26")
;; (set-face-attribute 'highlight-stages-level-2-face nil
;;                     :background "#001217")
;; (set-face-attribute 'highlight-stages-level-3-face nil
;;                     :background "#000608")
;; (set-face-attribute 'highlight-stages-higher-level-face nil
;;                     :background "#000000")




(provide 'init-my-emacs-highlight)

;;; init-my-emacs-highlight.el ends here
