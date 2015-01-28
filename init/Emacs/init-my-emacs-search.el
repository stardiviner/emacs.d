;;; init-my-emacs-search.el --- init search utilities for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Isearch ]

;;; Usage:
;;
;;   - `register-mark-mode' -- mark rectangle.
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


(setq query-replace-show-replacement t
      ;; query-replace-from-to-separator "->"
      )

;; (setq isearch-lazy-highlight-??)

(unless (boundp 'my-rectangle-map)
  (define-prefix-command 'my-rectangle-map))
(global-set-key (kbd "C-x r r") 'my-rectangle-map)

(global-unset-key (kbd "C-x r N"))
(global-unset-key (kbd "C-x r t"))
(global-unset-key (kbd "C-x r c"))
(global-unset-key (kbd "C-x r i"))
(global-unset-key (kbd "C-x r n"))
(global-unset-key (kbd "C-x r o"))
(global-unset-key (kbd "C-x r y"))
(global-unset-key (kbd "C-x r k"))
(global-unset-key (kbd "C-x r d"))
(global-unset-key (kbd "C-x r M-w"))

(define-key my-rectangle-map (kbd "r") 'rectangle-mark-mode)
(define-key my-rectangle-map (kbd "m") 'rectangle-mark-mode)
(define-key my-rectangle-map (kbd "c") 'copy-rectangle-to-register)
(define-key my-rectangle-map (kbd "M-w") 'copy-rectangle-as-kill)
(define-key my-rectangle-map (kbd "y") 'yank-rectangle)
(define-key my-rectangle-map (kbd "x") 'clear-rectangle)
(define-key my-rectangle-map (kbd "d") 'delete-rectangle)
(define-key my-rectangle-map (kbd "k") 'kill-rectangle)
(define-key my-rectangle-map (kbd "o") 'open-rectangle)
(define-key my-rectangle-map (kbd "t") 'string-rectangle)
(define-key my-rectangle-map (kbd "N") 'rectangle-number-lines)

;;; swap isearch with isearch-regexp.
;; replace [C-s] default (isearch-forward), press [M-r] to toggle isearch-regexp.
;; the default 'isearch-forward-regexp is bind to [C-M-s]
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
;; replace [M-%] default (query-replace)
(global-set-key (kbd "M-%") 'query-replace-regexp)


;;; [ Isearch+ ]

;;; Usage:

;; FIXME: caused eshell startup key prefix map C-c error.
;; (eval-after-load "isearch" '(require 'isearch+))

;; (setq isearchp-set-region-flag nil
;;       isearchp-restrict-to-region-flag t
;;       )



;;; [ visual-regexp ] -- A regexp/replace command for Emacs with interactive visual feedback.

;;; Usage:

(require 'visual-regexp)

;;; [ visual-regexp-steroids.el ] -- Extends visual-regexp to support other regexp engines.
(require 'visual-regexp-steroids)

(global-set-key (kbd "C-s") 'vr/isearch-forward)
(global-set-key (kbd "C-r") 'vr/isearch-backward)
(global-set-key (kbd "M-%") 'vr/replace)

(define-key my-search-prefix-map (kbd "r") 'vr/replace)
(define-key my-search-prefix-map (kbd "q") 'vr/query-replace)
;; if you use multiple-cursors interface, this is for you:
(if (featurep 'multiple-cursors)
    (define-key my-search-prefix-map (kbd "m") 'vr/mc-mark))
;; TODO: `vr/select-mc-mark', `vr/select-replace' etc.



;;; [ anzu ] -- Emacs Port of anzu.vim.

;;; provides a minor mode which displays current match and total matches
;;; information in the mode-line in various search mode.

;;; Usage:
;;; - [C-s] -- search
;;; - [M-%] -- query-replace
;;; - specified lines replacement :: [C-u 3 M-x anzu-query-replace]

(require 'anzu)

(setq anzu-regexp-search-commands '(vr/isearch-forward
                                    vr/isearch-backward
                                    isearch-forward-regexp
                                    isearch-backward-regexp)
      ;; anzu--mode-line-format '(:eval (anzu--update-mode-line)) ; add into my custom mode-line
      anzu--mode-line-format ""
      ;; anzu-cons-mode-line-p
      ;; anzu-input-idle-delay 0.05
      anzu-deactivate-region nil
      anzu-use-migemo nil
      anzu-replace-to-string-separator " ⇨ "
      ;; anzu-minimum-input-length 1
      )

(global-anzu-mode +1)
;; (anzu-mode +1)
(diminish 'anzu-mode)

;; (global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "M-%") 'anzu-query-replace-regexp)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

(set-face-attribute 'anzu-mode-line nil
                    :foreground "cyan"
                    :weight 'bold)
(set-face-attribute 'anzu-replace-highlight nil
                    )

;;; Function which constructs mode-line string. If you color mode-line string, you propertize string by yourself.
;; (defun my/anzu-update-func (here total)
;;   (propertize (format "<%d/%d>" here total)
;;               'face '((:foreground "yellow" :weight bold))))
;; (setq anzu-mode-line-update-function 'my/update-func)

;; (setq anzu-input-idle-delay 0.05)
;; (setq anzu-search-threshold 1000)
;; (setq anzu-regexp-search-commands '(isearch-forward-regexp isearch-backward-regexp))
;; (setq anzu-minimum-input-length 1)
;; (setq anzu-deactivate-region nil)


;;; [ Lazy Search ]

;; (require 'lazy-search)


;;; [ occur ]

(if (featurep 'helm)
    (define-key my-search-prefix-map (kbd "o") 'helm-occur))


;;; [ multi-occur ]

(if (featurep 'helm)
    (define-key my-search-prefix-map (kbd "O") 'helm-multi-occur))


;;; [ replace+.el ]


;;; [ Grep ]


;;; [ helm-grep ]

;; (setq helm-grep-default-command "grep -a -d skip %e -n%cH -e %p %f")

(define-key my-search-prefix-map (kbd "g") 'grep)


;;; [ Ack ]


;;; [ Full Ack ] -- An Emacs front-end for ack

;;; Usage:
;; - Run ack to search for all files and ack-same to search for files of the same
;;   type as the current buffer.
;; - next-error and previous-error can be used to jump to the matches.
;; - ack-find-file and ack-find-same-file use ack to list the files in the current
;;   project. It's a convenient, though slow, way of finding files.

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

(setq ack-and-a-half-use-ido t               ; use ido to provide completions
      ;; ack-and-a-half-executable "ack-grep"
      ;; ack-and-a-half-arguments ; extra arguments passed to ack
      ack-and-a-half-ignore-case 'smart
      ack-and-a-half-regexp-search t
      ack-and-a-half-regexp-history t
      ack-and-a-half-use-environment t
      ;; (ack-and-a-half-same)
      ;; ack-and-a-half-mode-type-default-alist
      ack-and-a-half-mode-type-alist nil
      ack-and-a-half-literal-history t
      ;; ack-and-a-half-prompt-for-directory 'unless-guessed
      ;; ack-and-a-half-root-directory-functions '(ack-and-a-half-guess-project-root)
      ack-and-a-half-prompt-for-directory t
      )

;; add more project root file patterns.
;; (add-to-list 'ack-and-a-half-project-root-file-patterns "\\.kk")

(unless (boundp 'ack-map)
  (define-prefix-command 'ack-map))
(define-key my-search-prefix-map (kbd "k") 'ack-map)

(define-key ack-map (kbd "k") 'ack)
(define-key ack-map (kbd "s") 'ack-same)
(define-key ack-map (kbd "f") 'ack-find-file)
(define-key ack-map (kbd "F") 'ack-find-file-same)


;;; [ silver search (ag) ] -- like ack, but faster.

;;; Usage:
;;
;;; Running a search:
;;
;; - ag
;; - ag-files
;; - ag-regexp
;; - ag-project
;; - ag-project-files
;; - ag-project-regexp
;;
;; - *-project :: commands automatically choose the directory to search, automatically
;;                detecting git, Subversion and Mercurial project roots.
;;
;; - *-regexp :: commands allow you to specify a PCRE pattern for your search term.
;;
;; - *-files :: commands allow you to specify a PCRE pattern for file names to
;;              search in. By default, ag searches in all files. Note that in both cases, ag
;;              ignores files that are ignored by your VCS (e.g. things mentioned in
;;              .gitignore).
;;
;;; Search for files:
;;
;; ag supports an option -g that lets you to list file names matching PCRE
;; patterns. It is analogical to find, but comes with all the nice features of
;; ag such as automatically ignoring all the vcs files. You can search for files
;; matching a pattern using functions
;; 
;; - ag-dired
;; - ag-dired-regexp
;; - ag-project-dired
;; - ag-project-dired-regexp

(require 'ag)

(setq ag-highlight-search t
      ag-reuse-buffers 't
      ag-reuse-window nil ; nil, or 't. (I use value `nil' for popwin to capture)
      ;; ag-arguments
      )

(define-key my-search-prefix-map (kbd "s") 'ag-regexp-project-at-point) ; 'ag, 'ag-regexp, 


;;; [ helm-ag ]

;;; Usage:
;;
;; - helm-ag :: Input search word with ag command. You can change search directory with C-u prefix.
;; - helm-ag-this-file :: Same as helm-ag except to search only current file
;; - helm-do-ag :: Search with ag like helm-do-grep.
;; - helm-ag-pop-stack :: Move to point before jump
;; - helm-ag-clear-stack :: Clear context stack
;; - Helm persistent action :: You can see file content temporarily by persistent action(C-z) at helm-ag and helm-ag-this-file.

(require 'helm-ag)

(setq helm-ag-insert-at-point 'word ; same thing as `thing-at-point' such ash: 'word, symbol,
      helm-ag-base-command "ag --nocolor --nogroup" ; helm use color match, so use option `--nocolor' here.
      ;; helm-ag-command-option
      helm-ag-source-type 'one-line ; 'one-line, 'file-line
      )

;; You can use `helm-ag' with projectile by following command.
(require 'projectile)
(defun projectile-helm-ag ()
  "Use helm-ag to be more better ag search with helm."
  (interactive)
  (helm-ag (projectile-project-root)))
(define-key projectile-command-map (kbd "s h") 'projectile-helm-ag)

(define-key my-search-prefix-map (kbd "a") 'helm-ag)



;;; [ awk-it ] -- run AWK interactively on region!

;;; Usage:
;;
;; - `awk-it-' prefix
;; - [M-x awk-it]

;; (require 'awk-it)

;; (define-key my-search-prefix-map (kbd "w") 'awk-it)


;;; [ migemo ] -- provides Japanese increment search with 'Romanization of Japanese'(ローマ字).

;; https://github.com/emacs-jp/migemo


(provide 'init-my-emacs-search)

;;; init-my-emacs-search.el ends here
