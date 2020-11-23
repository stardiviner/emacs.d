;;; init-emacs-unicode.el --- init for Unicode edit and display.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ ucs-utils ] --- utilities unicode characters in Emacs.

;;; Usage:
;;
;;; Quickstart
;;
;; (ucs-utils-char "Middle Dot"         ; character to return
;;                 ?.                   ; fallback if unavailable
;;                 'char-displayable-p) ; test for character to pass
;;
;; (ucs-utils-first-existing-char '("White Bullet"
;;                                  "Bullet Operator"
;;                                  "Circled Bullet"
;;                                  "Middle Dot"
;;                                  ?.) 'cdp)
;;
;; (ucs-utils-string "Horizontal Ellipsis" '[["..."]])

(require 'ucs-utils)

;; (ucs-utils-char "arrow →"               ; character to return
;;                 ?→                      ; fallback if unavailable
;;                 'char-displayable-p      ; test for character to pass
;;                 )

(define-key global-map [remap insert-char] 'ucs-utils-ucs-insert) ; [C-x 8 RET]

;;; [ insert-char-preview ] -- Insert Unicode char via insert-char-preview
;;; similar to insert-char command, but with character preview in completion
;;; prompt.

(use-package insert-char-preview
  :ensure t
  :commands (insert-char-preview)
  :bind ("C-x 8 RET" . insert-char-preview))

(provide 'init-emacs-unicode)

;;; init-emacs-unicode.el ends here
