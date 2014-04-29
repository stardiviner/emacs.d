;;; init-my-emacs-outline.el --- init Emacs for outline.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ allout ]

;;; allout outline mode provides extensive outline formatting and manipulation
;;; beyond standard emacs outline mode. It provides for structured editing of
;;; outlines, as well as navigation and exposure. It also provides for
;;; syntax-sensitive text like programming languages. (For an example, see the
;;; allout code itself, which is organized in an allout outline framework.)

;;; In addition to outline navigation and exposure, allout includes:

;; - topic-oriented creation, promotion, demotion, cut/paste across depths and
;;   locations, etc.
;;
;; - pgp symmetric and key-pair topic encryption, so any outline can include any
;;   combination of encrypted and plain-text topics
;;
;; - configurable, per-file initial exposure settings
;;
;; - incremental search with dynamic exposure and reconcealment of hidden text
;;
;; - automatic topic-number maintenance of numbered (’#’ bullet) topics
;;
;; - hot-spot operation, for single-keystroke maneuvering and exposure
;;   control. (See the outline-mode docstring.)

;;; Usage:
;; Help:
;; - [C-h f allout-mode] ::
;; - [C-c SPC] :: prefix of allout keybindings.
;; - [C-c SPC C-s] :: show entry.
;; - [C-c SPC h] :: hide entry.

(require 'allout)

(setq allout-auto-activation t
      allout-command-prefix (kbd "C-c SPC")          ; "\C-c "
      ;; allout-prefixed-keybindings [(control ?a)] ; ?a, ?. [
      )

;; To use the allout package in place of the standard outline package, add the
;; following bit of code.
;; (require 'outline "allout")

;;; variables:
(setq allout-auto-activation t
      allout-widgets-auto-activation t
      ;; allout-layout '(0 : -1 -1 0)
      )


;; ;; -------------------------------------------------------------------------------
;; ;; By default, allout mode does not fontify the buffer. To get Font Lock to work
;; ;; put the following into your initialization file (adapted from the standard
;; ;; outline mode):
;; (defvar rf-allout-font-lock-keywords
;;   '(;;
;;     ;; Highlight headings according to the level.
;;     (eval . (list (concat "^\\(" outline-regexp "\\).+")
;;                   0 '(or (cdr (assq (outline-depth)
;;                                     '((1 . font-lock-function-name-face)
;;                                       (2 . font-lock-variable-name-face)
;;                                       (3 . font-lock-keyword-face)
;;                                       (4 . font-lock-builtin-face)
;;                                       (5 . font-lock-comment-face)
;;                                       (6 . font-lock-constant-face)
;;                                       (7 . font-lock-type-face)
;;                                       (8 . font-lock-string-face))))
;;                          font-lock-warning-face)
;;                   nil t)))
;;   "Additional expressions to highlight in Outline mode.")

;; ;; add font-lock to allout mode
;; (defun rf-allout-font-lock-hook ()
;;   (set (make-local-variable 'font-lock-defaults)
;;        '(rf-allout-font-lock-keywords t nil nil outline-back-to-current-heading)))

;; (add-hook 'outline-mode-hook 'rf-allout-font-lock-hook)
;; ;; -------------------------------------------------------------------------------

(global-set-key (kbd "M-n") 'allout-next-heading)
(global-set-key (kbd "M-p") 'allout-previous-heading)

(allout-minor-mode 1)
(diminish 'allout-mode)


(provide 'init-my-emacs-outline)

;;; init-my-emacs-outline.el ends here
