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

(require 'isearch+)

(setq isearchp-set-region-flag nil
      isearchp-restrict-to-region-flag t
      ;; isearchp-restrict-to-region-flag
      )


;;; [ Lazy Search ]

;; (require 'lazy-search)


;;; [ Occur Mode ]


;;; [ visual-regexp ] -- A regexp/replace command for Emacs with interactive visual feedback.

;;; Usage:
;;; [C-c C-s] + [r/q/m]

(require 'visual-regexp)

(define-key my-search-prefix-map (kbd "r") 'vr/replace)
(define-key my-search-prefix-map (kbd "q") 'vr/query-replace)
;; if you use multiple-cursors, this is for you:
;; TODO: add an condition here after multiple-curses loaded.
(define-key my-search-prefix-map (kbd "m") 'vr/mc-mark)


;;; [ visual-regexp-steroids.el ] -- Extends visual-regexp to support other regexp engines.

(require 'visual-regexp-steroids)

;; to use visual-regexp-steroids's isearch instead of the built-in regexp isearch, also include the following lines:
;; (define-key esc-map (kbd "C-r") 'vr/isearch-backward) ;; C-M-r
;; (define-key esc-map (kbd "C-s") 'vr/isearch-forward) ;; C-M-s


;;; [ replace+.el ]


;;; [ Grep ]


;;; [ Ack ]


;;; [ Full Ack ] -- An Emacs front-end for ack

;;; Usage:
;; - Run ack to search for all files and ack-same to search for files of the same
;;   type as the current buffer.
;; - next-error and previous-error can be used to jump to the matches.
;; - ack-find-file and ack-find-same-file use ack to list the files in the current
;;   project. It's a convenient, though slow, way of finding files.

;; TODO:

;; (unless (package-installed-p 'full-ack)
;;   (package-install 'full-ack))
;; (require 'full-ack)

;; (autoload 'ack-same "full-ack" nil t)
;; (autoload 'ack "full-ack" nil t)
;; (autoload 'ack-find-same-file "full-ack" nil t)
;; (autoload 'ack-find-file "full-ack" nil t)


;;; [ ack-and-a-half ]

(require 'ack-and-a-half)

;; Create shorter aliases
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

(setq ack-and-a-half-executable "ack-grep"
      ack-and-a-half-use-ido t               ; use ido to provide completions
      ;; ack-and-a-half-arguments ; extra arguments passed to ack
      ack-and-a-half-ignore-case 'smart
      ack-and-a-half-regexp-search t
      ack-and-a-half-regexp-history t
      ack-and-a-half-use-environment t
      ;; (ack-and-a-half-same)
      ;; ack-and-a-half-mode-type-default-alist
      ack-and-a-half-mode-type-alist nil
      ack-and-a-half-literal-history t
      ;; ack-and-a-half-root-directory-functions '(ack-and-a-half-guess-project-root)
      ack-and-a-half-prompt-for-directory 'unless-guessed
      ;; TODO add more project root file patterns.
      ack-and-a-half-project-root-file-patterns '(".project\\'"
                                                  "\\`.git\\'" "\\`.bzr\\'" "\\`_darcs\\'" "\\`.hg\\'"
                                                  ".xcodeproj\\'" ".sln\\'" "\\`Project.ede\\'"
                                                  )

      )


(define-key my-search-prefix-map (kbd "a") 'ack-and-a-half)


;;; [ Ace Jump Mode ]

;;; Usage:
;; "C-c SPC" ==> ace-jump-word-mode
;;     enter first character of a word, select the highlighted key to move to it.
;; "C-u C-c SPC" ==> ace-jump-char-mode
;;     enter a character for query, select the highlighted key to move to it.
;; "C-u C-u C-c SPC" ==> ace-jump-line-mode
;;     each non-empty line will be marked, select the highlighted key to move to it.

(require 'ace-jump-mode)

(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; enable a more powerful jump back function from ace jump mode
(autoload 'ace-jump-mode-pop-mark "ace-jump-mode" "Ace jump back:-)" t)
(eval-after-load "ace-jump-mode" '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)


;;; Search Keybind

;; Occur search the full list of keybinds & their commands. Very helpful for
;; learning and remembering forgotten binds.

(defun search-keybind (regexp &optional nlines)
  (interactive (occur-read-primary-args))
  (save-excursion
    (describe-bindings)
    (set-buffer "*Help*")
    (occur regexp nlines)
    (delete-windows-on "*Help*")))

(define-key my-search-prefix-map (kbd "k") 'search-keybind)



(provide 'init-my-emacs-search)

;;; init-my-emacs-search.el ends here
