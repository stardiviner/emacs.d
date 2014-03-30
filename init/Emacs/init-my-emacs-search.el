;;; init-my-emacs-search.el --- init search utilities for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; keybindings prefix

(define-prefix-command 'my-search-prefix-map)
(global-set-key (kbd "C-c s") 'my-search-prefix-map)


;;; [ Isearch ]

;;; Usage:
;;
;; - Quitting Isearch – Many KeySequences quit Isearch. One that many people use is
;;   [RET]. You cannot use [RET] to search for the end of a line – use [C-q C-j]
;;   for that. The cursor is left on the last match.
;; - [C-g] – Abort the search, putting back the cursor to its initial position.
;; - [C-s] – Repeat the search as many times as you want throughout the buffer.
;; - [C-w] – Select the (rest of the) word the TextCursor is on as the search
;;   string; repeating [C-w] appends more words to the search string.
;; - [M-s C-e] – Select the text up to the end of the line as the search string
;;   (this was bound to [C-y] up until Emacs 24.1).
;; - [M-s h r] – Highlight regular expression ([highlight-regexp])
;; - [M-s h u] – Unhighlight regular expression
;; - [M-s o] – call [occur] with the current search term
;; - [C-y] – Yank (paste) the text last copied to the kill-buffer (clipboard) to
;;   the end of the search string (this was bound to [M-y] up until Emacs 24.1).
;; - [M-n], [M-p] – Re-use a previous search string. Repeat to choose older
;;   strings. [M-n] moves forward in the search history; [M-p] moves backward.
;; - [M-TAB] – Complete the current search string against all previous search
;;   strings.
;; - [M-c] – Toggle search case-sensitivity.
;; - [M-r] – Toggle between regular-expression searching and literal-string
;;   searching.
;; - [M-e] – Pause to edit the search string. Searching is disabled until you
;;   explicitly resume it with [C-j] (or [C-s] or [C-r]).
;; - [M-%] – Start query replace using the current string.
;; - [C-M-%] – Start a regular-expression query replace using the current search
;;   string.


;;; swap isearch with isearch-regexp.
;; replace [C-s] default (isearch-forward), press [M-r] to toggle isearch-regexp.
;; the default 'isearch-forward-regexp is bind to [C-M-s]
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
;; replace [M-%] default (query-replace)
(global-set-key (kbd "M-%") 'query-replace-regexp)


;;; [ Isearch+ ]

;;; Usage:

(unless (package-installed-p 'isearch+)
  (package-install  'isearch+))
(require 'isearch+)

(setq isearchp-set-region-flag nil
      isearchp-restrict-to-region-flag t
      ;; isearchp-restrict-to-region-flag
      )




(provide 'init-my-emacs-search)

;;; init-my-emacs-search.el ends here
