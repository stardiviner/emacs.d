;;; init-my-emacs-highlight.el ---
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
;; highlight-symbol-query-replace can be used to replace the symbol.

(require 'highlight-symbol)

;; (global-set-key [(control f3)] 'highlight-symbol-at-point)
;; (global-set-key [f3] 'highlight-symbol-next)
;; (global-set-key [(shift f3)] 'highlight-symbol-prev)
;; (global-set-key [(meta f3)] 'highlight-symbol-query-replace)

(setq highlight-symbol-idle-delay 1.5
      highlight-symbol-border-pattern '("\\_<" . "\\_>")
      highlight-symbol-colors '("yellow" "DeepPink" "cyan"
                                "MediumPurple1" "SpringGreen1" "DarkOrange" "HotPink1"
                                "RoyalBlue1" "OliveDrab")
      ;; highlight-symbol-foreground-color
      )

(set-face-attribute 'highlight-symbol-face nil
                    :foreground nil :background "midnight blue"
                    :slant 'italic)

(add-hook 'prog-mode-hook 'highlight-symbol-mode)

(define-key my-search-prefix-map (kbd "h") 'highlight-symbol-at-point)
;;; highlight-quoted

(load-file (expand-file-name "init/extensions/highlight-quoted.el" user-emacs-directory))

(add-hook 'prog-mode-hook 'highlight-quoted-mode)

;; (setq highlight-quoted-highlight-symbols t)

(eval-after-load 'highlight-quoted
  (progn
    (set-face-attribute 'highlight-quoted-quote nil
                        :inherit 'font-lock-keyword-face)
    (set-face-attribute 'highlight-quoted-symbol nil
                        :inherit 'font-lock-constant-face))
  )


;;; highlight-numbers

(load-file (expand-file-name "init/extensions/highlight-numbers.el" user-emacs-directory))

(add-hook 'prog-mode-hook 'highlight-numbers-mode)

;; (setq highlight-numbers-modelist)

(eval-after-load 'highlight-numbers
  (set-face-attribute 'highlight-numbers-number nil
                      :inherit 'font-lock-constant-face))




(provide 'init-my-emacs-highlight)

;;; init-my-emacs-highlight.el ends here
