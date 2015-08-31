;;; init-my-emacs-outline.el --- init Emacs for outline.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'my-outline-prefix)
  (define-prefix-command 'my-outline-prefix))
(global-set-key (kbd "C-c SPC") 'my-outline-prefix)
(global-set-key (kbd "C-c @") 'my-outline-prefix)


;;; [ outline-mode ]

;;; Usage:
;; - [C-c @ C-a] -- show all
;; - [C-c @ C-t] -- show only the headings
;; - [C-c @ C-s] -- show subtree at cursor location
;; - [C-c @ C-d] -- hide subtree at cursor location

;; (setq outline-regexp "[*\f]+")

;; ;;; ----------- Vim-type outline marker ------------
;; ;;
;; ;; Vim editor has several different outlining (called folding) options. One of
;; ;; them is manual outlining in which user inserts predefined markers to the text
;; ;; and the outline level is determined by the markers. The default markers are
;; ;; {{{ and }}} for starting and ending a fold. A number after the marker
;; ;; determines the outline level (e.g., {{{1, {{{2, {{{3 and so on).
;; ;;
;; ;; The following code adds an interactive function ‘set-vim-foldmarker’ which
;; ;; can be used to set Vim-type opening outline marker for the current buffer.

;; (defun set-vim-foldmarker (fmr)
;;   "Set Vim-type foldmarkers for the current buffer"
;;   (interactive "sSet local Vim foldmarker: ")
;;   (if (equal fmr "")
;;       (message "Abort")
;;     (setq fmr (regexp-quote fmr))
;;     (set (make-local-variable 'outline-regexp)
;;          (concat ".*" fmr "\\([0-9]+\\)"))
;;     (set (make-local-variable 'outline-level)
;;          `(lambda ()
;;             (save-excursion
;;               (re-search-forward
;;                ,(concat fmr "\\([0-9]+\\)") nil t)
;;               (string-to-number (match-string 1)))))))

;; ;;; While in outline-mode or outline-minor-mode you can call this interactively
;; ;;; with ‘M-x set-vim-foldmarker’ and enter the opening marker when asked. If
;; ;;; you use outline-minor-mode and Vim-type outlining always with certain major
;; ;;; mode you can add the following lines to the appropriate major mode hook:

;; (outline-minor-mode 1)
;; (set-vim-foldmarker "{{{")
;; ;;; -----------------------------------------------------------------------




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

;;; ------------ custom keybindings --------------


;; ;; Outline-minor-mode key map
;; (define-prefix-command 'cm-map nil "Outline-")
;; ;; HIDE
;; (define-key cm-map "q" 'hide-sublevels)    ; Hide everything but the top-level headings
;; (define-key cm-map "t" 'hide-body)         ; Hide everything but headings (all body lines)
;; (define-key cm-map "o" 'hide-other)        ; Hide other branches
;; (define-key cm-map "c" 'hide-entry)        ; Hide this entry's body
;; (define-key cm-map "l" 'hide-leaves)       ; Hide body lines in this entry and sub-entries
;; (define-key cm-map "d" 'hide-subtree)      ; Hide everything in this entry and sub-entries
;; ;; SHOW
;; (define-key cm-map "a" 'show-all)          ; Show (expand) everything
;; (define-key cm-map "e" 'show-entry)        ; Show this heading's body
;; (define-key cm-map "i" 'show-children)     ; Show this heading's immediate child sub-headings
;; (define-key cm-map "k" 'show-branches)     ; Show all sub-headings under this heading
;; (define-key cm-map "s" 'show-subtree)      ; Show (expand) everything in this heading & below
;; ;; MOVE
;; (define-key cm-map "u" 'outline-up-heading)                ; Up
;; (define-key cm-map "n" 'outline-next-visible-heading)      ; Next
;; (define-key cm-map "p" 'outline-previous-visible-heading)  ; Previous
;; (define-key cm-map "f" 'outline-forward-same-level)        ; Forward - same level
;; (define-key cm-map "b" 'outline-backward-same-level)       ; Backward - same level
;; (global-set-key "\M-o" cm-map)


;; ;;; Another solution is to just define a hook for the minor mode that just
;; ;;; defines an additional prefix key.

;; (add-hook 'outline-minor-mode-hook
;;           (lambda () (local-set-key "\C-c\C-c"
;;                                outline-mode-prefix-map)))


;; (defun body-p ()
;;   (save-excursion
;;     (outline-back-to-heading)
;;     (outline-end-of-heading)
;;     (and (not (eobp))
;;          (progn (forward-char 1)
;;                 (not (outline-on-heading-p))))))

;; (defun body-visible-p ()
;;   (save-excursion
;;     (outline-back-to-heading)
;;     (outline-end-of-heading)
;;     (outline-visible)))

;; (defun subheadings-p ()
;;   (save-excursion
;;     (outline-back-to-heading)
;;     (let ((level (funcall outline-level)))
;;       (outline-next-heading)
;;       (and (not (eobp))
;;            (< level (funcall outline-level))))))

;; (defun subheadings-visible-p ()
;;   (interactive)
;;   (save-excursion
;;     (outline-next-heading)
;;     (outline-visible)))

;; (defun outline-do-close ()
;;   (interactive)
;;   (if (outline-on-heading-p)
;;       (cond ((and (body-p) (body-visible-p))
;;              (hide-entry))
;;             ((and (subheadings-p)
;;                   (subheadings-visible-p))
;;              (hide-subtree))
;;             (t (outline-previous-visible-heading 1)))
;;     (outline-back-to-heading t)))

;; (defun outline-do-open ()
;;   (interactive)
;;   (if (outline-on-heading-p)
;;       (cond ((and (subheadings-p)
;;                   (not (subheadings-visible-p)))
;;              (show-children))
;;             ((and (body-p)
;;                   (not (body-visible-p)))
;;              (show-entry))
;;             (t (show-entry)))
;;     (outline-next-visible-heading 1)))

;; (define-key outline-mode-map '[left] 'outline-do-close)
;; (define-key outline-mode-map '[right] 'outline-do-open)
;; (define-key outline-minor-mode-map '[left] 'outline-do-close)
;; (define-key outline-minor-mode-map '[right] 'outline-do-open)


;; Using Meta + <arrow key>

;; This is a variation of the above Explorer-like keybindings.

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; outline-mode-easy-bindings.el (2010-08-15)
;; ;;
;; ;; Outlines can be managed entirely with Meta + <arrow key>.
;; ;;
;; ;; Installation: Store this file as outline-mode-easy-bindings.el
;; ;; somewhere in your load-path and create hooks for outline modes to
;; ;; load this automatically, for example:

;; ;;     (add-hook 'outline-mode-hook 'my-outline-easy-bindings)
;; ;;     (add-hook 'outline-minor-mode-hook 'my-outline-easy-bindings)
;; ;;
;; ;;     (defun my-outline-easy-bindings ()
;; ;;       (require 'outline-mode-easy-bindings nil t))


;; (defun outline-body-p ()
;;   (save-excursion
;;     (outline-back-to-heading)
;;     (outline-end-of-heading)
;;     (and (not (eobp))
;;          (progn (forward-char 1)
;;                 (not (outline-on-heading-p))))))

;; (defun outline-body-visible-p ()
;;   (save-excursion
;;     (outline-back-to-heading)
;;     (outline-end-of-heading)
;;     (not (outline-invisible-p))))

;; (defun outline-subheadings-p ()
;;   (save-excursion
;;     (outline-back-to-heading)
;;     (let ((level (funcall outline-level)))
;;       (outline-next-heading)
;;       (and (not (eobp))
;;            (< level (funcall outline-level))))))

;; (defun outline-subheadings-visible-p ()
;;   (interactive)
;;   (save-excursion
;;     (outline-next-heading)
;;     (not (outline-invisible-p))))

;; (defun outline-hide-more ()
;;   (interactive)
;;   (when (outline-on-heading-p)
;;     (cond ((and (outline-body-p)
;;                 (outline-body-visible-p))
;;            (hide-entry)
;;            (hide-leaves))
;;           (t
;;            (hide-subtree)))))

;; (defun outline-show-more ()
;;   (interactive)
;;   (when (outline-on-heading-p)
;;     (cond ((and (outline-subheadings-p)
;;                 (not (outline-subheadings-visible-p)))
;;            (show-children))
;;           ((and (not (outline-subheadings-p))
;;                 (not (outline-body-visible-p)))
;;            (show-subtree))
;;           ((and (outline-body-p)
;;                 (not (outline-body-visible-p)))
;;            (show-entry))
;;           (t
;;            (show-subtree)))))

;; (let ((map outline-mode-map))
;;   (define-key map (kbd "M-<left>") 'outline-hide-more)
;;   (define-key map (kbd "M-<right>") 'outline-show-more)
;;   (define-key map (kbd "M-<up>") 'outline-previous-visible-heading)
;;   (define-key map (kbd "M-<down>") 'outline-next-visible-heading)
;;   (define-key map (kbd "C-c J") 'outline-hide-more)
;;   (define-key map (kbd "C-c L") 'outline-show-more)
;;   (define-key map (kbd "C-c I") 'outline-previous-visible-heading)
;;   (define-key map (kbd "C-c K") 'outline-next-visible-heading))

;; (let ((map outline-minor-mode-map)) 
;;   (define-key map (kbd "M-<left>") 'outline-hide-more)
;;   (define-key map (kbd "M-<right>") 'outline-show-more)
;;   (define-key map (kbd "M-<up>") 'outline-previous-visible-heading)
;;   (define-key map (kbd "M-<down>") 'outline-next-visible-heading)
;;   (define-key map (kbd "C-c J") 'outline-hide-more)
;;   (define-key map (kbd "C-c L") 'outline-show-more)
;;   (define-key map (kbd "C-c I") 'outline-previous-visible-heading)
;;   (define-key map (kbd "C-c K") 'outline-next-visible-heading))

;; (provide 'outline-mode-easy-bindings)


;; Another Outline Binding To Cursor Keys

;; My bindings use ALT + arrows and CTRL + ALT + arrows to navigate outlines:

;; (global-set-key [M-left] 'hide-body)
;; (global-set-key [M-right] 'show-all)
;; (global-set-key [M-up] 'outline-previous-heading)
;; (global-set-key [M-down] 'outline-next-heading)
;; (global-set-key [C-M-left] 'hide-sublevels)
;; (global-set-key [C-M-right] 'show-children)
;; (global-set-key [C-M-up] 'outline-previous-visible-heading)
;; (global-set-key [C-M-down] 'outline-next-visible-heading)





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

;; so that you can active/inactive allout-minor-mode to edit/navigate/folding with it.
;; (define-key my-edit-prefix (kbd "o") 'allout-minor-mode)
;; activate outline mode for current buffer, and establish a default file-var setting for `allout-layout'.
(defun my-allout-toggle ()
  "Toggle allout for current buffer."
  (interactive)
  (if (allout-mode-p)
      (allout-mode -1)
    (outlineify-sticky)
    (allout-hide-bodies)
    (define-key allout-mode-map (kbd "C-c SPC C-l") 'allout-hide-bodies)))

;; (define-key my-edit-prefix (kbd "o") 'outlineify-sticky)
(define-key my-edit-prefix (kbd "o") 'my-allout-toggle)

;; (unless (boundp 'my-outline-prefix)
;;   (define-prefix-command 'my-outline-prefix))
;; (define-key my-edit-prefix (kbd "o") 'my-outline-prefix)
;;
;; (define-key my-outline-prefix (kbd "n") 'allout-next-heading)


;;; [ origami ]

;;; `global-origami-mode' & `origami-mode'

(use-package origami
  :config
  (define-key my-outline-prefix (kbd "m") 'origami-mode)
  (define-key my-outline-prefix (kbd "SPC") 'origami-toggle-node)
  (define-key my-outline-prefix (kbd "TAB") 'origami-toggle-all-nodes)
  (define-key my-outline-prefix (kbd "n") 'origami-next-fold)
  (define-key my-outline-prefix (kbd "p") 'origami-previous-fold)
  (define-key my-outline-prefix (kbd "c") 'origami-close-node)
  (define-key my-outline-prefix (kbd "C") 'origami-close-all-nodes)
  (define-key my-outline-prefix (kbd "o") 'origami-open-node)
  (define-key my-outline-prefix (kbd "O") 'origami-open-all-nodes)
  (define-key my-outline-prefix (kbd "T") 'origami-recursively-toggle-node)
  (define-key my-outline-prefix (kbd ">") 'origami-open-node-recursively)
  (define-key my-outline-prefix (kbd "<") 'origami-close-node-recursively)
  (define-key my-outline-prefix (kbd "O") 'origami-show-only-node)
  (define-key my-outline-prefix (kbd "u") 'origami-undo)
  (define-key my-outline-prefix (kbd "r") 'origami-redo)
  (define-key my-outline-prefix (kbd "!") 'origami-reset)
  )


(provide 'init-my-emacs-outline)

;;; init-my-emacs-outline.el ends here
