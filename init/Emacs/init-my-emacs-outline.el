;;; init-my-emacs-outline.el --- init Emacs for outline.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ outline-mode ]

;;; Usage:
;; - [C-c @ C-a] -- show all
;; - [C-c @ C-t] -- show only the headings
;; - [C-c @ C-s] -- show subtree at cursor location
;; - [C-c @ C-d] -- hide subtree at cursor location



;;; [ outline-minor-more ]

;;; Usage:
;; - [C-c @] -- prefix.

;; - [M-x outline-minor-mode]       --	enable Outline minor mode
;; - [C-c @ C-t]                    --	hide all of buffer except headings
;; - [C-c @ C-a]                    --	show all of the text in the buffer
;; - [C-c @ C-q]                    --	hide everything but top levels headers
;; - [C-c @ TAB]                    --	show all direct subheadings of this heading
;; - [C-c @ C-k]                    --	show all subheadings, but not bodies
;; - [M-x outline-previous-heading] --	go to previous heading
;; - [M-x outline-next-heading]     --	go to next heading
;; - [C-c @ C-p]                    --	go to previous visible heading
;; - [C-c @ C-n]                    --	go to next visible heading


;;; [ Outline Magic ] -- it extends outline-mode and outline-minor-mode with a single command to toggle hiding/showing (outline-cycle) which you can bind to something like <C-tab>.


;;; [ allout-mode ]

;;; allout outline mode provides extensive outline formatting and manipulation
;;; beyond standard emacs outline mode. It provides for structured editing of
;;; outlines, as well as navigation and exposure. It also provides for
;;; syntax-sensitive text like programming languages. (For an example, see the
;;; allout code itself, which is organized in an allout outline framework.)
;;;
;;; In addition to outline navigation and exposure, allout includes:
;;
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
;; - [C-h v allout-prefixed-keybindings] + [C-h v allout-unprefixed-keybindings]

(require 'allout)

;; To use the allout package in place of the standard outline package, add the
;; following bit of code.
;; (require 'outline "allout")

(setq allout-auto-activation t
      ;; allout-layout
      allout-default-layout '(-2 : -1 *)
      ;; [buffer-local] allout-layout '(0 : -1 -1 0)
      allout-widgets-auto-activation t
      allout-command-prefix (kbd "C-c SPC")
      )
(setq-default allout-use-mode-specific-leader nil
              allout-stylish-prefixes t
              allout-primary-bullet "*" ; used by level-1
              allout-header-prefix "."
              allout-distinctive-bullets-string "*+-=>()[{}&!?#%\"X@$~_\\:;^"
              allout-plain-bullets-string-len 5
              allout-plain-bullets-string "*+#>." ; + -> #N -> > -> *
              )



;; so that you can active/inactive allout-minor-mode to edit/navigate/folding with it.
;; (define-key my-edit-prefix-map (kbd "o") 'allout-minor-mode)
;; activate outline mode for current buffer, and establish a default file-var setting for `allout-layout'.
(define-key my-edit-prefix-map (kbd "o") 'outlineify-sticky)


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
;;
;; ;; add font-lock to allout mode
;; (defun rf-allout-font-lock-hook ()
;;   (set (make-local-variable 'font-lock-defaults)
;;        '(rf-allout-font-lock-keywords t nil nil outline-back-to-current-heading)))
;;
;; (add-hook 'outline-mode-hook 'rf-allout-font-lock-hook)
;; ;; -------------------------------------------------------------------------------


(allout-minor-mode 1)
(diminish 'allout-mode)

;; (unless (boundp 'my-outline-prefix-map)
;;   (define-prefix-command 'my-outline-prefix-map))
;; (define-key my-edit-prefix-map (kbd "o") 'my-outline-prefix-map)
;;
;; (define-key my-outline-prefix-map (kbd "n") 'allout-next-heading)


;;; [ Folding ]

;;; folding-mode --- 


(provide 'init-my-emacs-outline)

;;; init-my-emacs-outline.el ends here
