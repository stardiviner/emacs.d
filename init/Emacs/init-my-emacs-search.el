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
      isearch-allow-scroll t
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

(unless (boundp 'my-isearch-prefix)
  (define-prefix-command 'my-isearch-prefix))
(define-key my-search-prefix (kbd "i") 'my-isearch-prefix)

(define-key my-isearch-prefix (kbd "i") 'isearch-forward)
(define-key my-isearch-prefix (kbd "I") 'isearch-forward-regexp)
(define-key my-isearch-prefix (kbd "f") 'isearch-forward)
(define-key my-isearch-prefix (kbd "F") 'isearch-forward-regexp)
(define-key my-isearch-prefix (kbd "b") 'isearch-backward)
(define-key my-isearch-prefix (kbd "B") 'isearch-backward-regexp)
(define-key my-isearch-prefix (kbd "o") 'isearch-occur)
(define-key my-isearch-prefix (kbd "r") 'vr/isearch-forward)
(define-key my-isearch-prefix (kbd "R") 'vr/isearch-backward)


;;; custom function

;;; custom isearch to start from buffer beginning
;; TODO: use `advise' for this function.
;; (defun my-isearch-forward ()
;;   (interactive)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (isearch-forward)))

;;; smart delete/backspace in isearch

(defun isearch-smart-delete ()
  "Delete the failed portion of the search string, or the last char if successful."
  (interactive)
  (with-isearch-suspended
   (setq isearch-new-string
         (substring
          isearch-string 0 (or (isearch-fail-pos) (1- (length isearch-string))))
         isearch-new-message
         (mapconcat 'isearch-text-char-description isearch-new-string ""))))

(define-key isearch-mode-map (kbd "<backspace>") 'isearch-smart-delete)
(define-key isearch-mode-map (kbd "DEL") 'isearch-smart-delete)

;; or from isearch+ use [M-s e]
(setq isearchp-drop-mismatch t)


;;; [ Isearch+ ]

;;; Usage:

;; FIXME: caused eshell startup key prefix map C-c error.
;; (eval-after-load "isearch" '(require 'isearch+))

;; (setq isearchp-set-region-flag nil
;;       isearchp-restrict-to-region-flag t
;;       )



;;; [ visual-regexp ] -- A regexp/replace command for Emacs with interactive visual feedback.

;;; Usage:

;;; [ visual-regexp-steroids.el ] -- Extends visual-regexp to support other regexp engines.

(global-set-key (kbd "C-s") 'vr/isearch-forward)
(global-set-key (kbd "C-r") 'vr/isearch-backward)
(global-set-key (kbd "M-%") 'vr/replace)

(define-key my-search-prefix (kbd "r") 'vr/replace)
(define-key my-search-prefix (kbd "q") 'vr/query-replace)
;; if you use multiple-cursors interface, this is for you:
(if (featurep 'multiple-cursors)
    (define-key my-search-prefix (kbd "m") 'vr/mc-mark))
;; TODO: `vr/select-mc-mark', `vr/select-replace' etc.



;;; [ anzu ] -- Emacs Port of anzu.vim.

;;; provides a minor mode which displays current match and total matches
;;; information in the mode-line in various search mode.

(use-package anzu
  :config
  (setq anzu-regexp-search-commands '(vr/isearch-forward
                                      vr/isearch-backward
                                      isearch-forward-regexp
                                      isearch-backward-regexp)
        anzu-cons-mode-line-p nil
        ;; anzu--mode-line-format '(:eval (anzu--update-mode-line)) ; add into my custom mode-line
        ;; anzu--mode-line-format ""
        ;; anzu-input-idle-delay 0.05
        anzu-deactivate-region nil
        anzu-use-migemo nil
        anzu-replace-to-string-separator " ⇨ "
        ;; anzu-minimum-input-length 1
        ;; anzu-search-threshold nil ; limit of search number.
        )

  ;; Function which constructs mode-line string. If you color mode-line string, you propertize string by yourself.
  ;; (defun my/anzu-update-func (here total)
  ;;   (propertize (format "<%d/%d>" here total)
  ;;               'face '((:foreground "yellow" :weight bold))))
  ;; (defun anzu--update-mode-line-default (here total)
  ;;   (when anzu--state
  ;;     (let ((status (cl-case anzu--state
  ;;                     (search (format "(%s/%d%s)"
  ;;                                     (anzu--format-here-position here total)
  ;;                                     total (if anzu--overflow-p "+" "")))
  ;;                     (replace-query (format "(%d replace)" total))
  ;;                     (replace (format "(%d/%d)" here total)))))
  ;;       (propertize status 'face 'anzu-mode-line))))
  ;; (setq anzu-mode-line-update-function 'my/update-func)

  
  ;; (global-set-key (kbd "M-%") 'anzu-query-replace)
  (global-set-key (kbd "M-%") 'anzu-query-replace-regexp)
  (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

  (set-face-attribute 'anzu-mode-line nil
                      :foreground "cyan"
                      :weight 'bold)
  (set-face-attribute 'anzu-replace-highlight nil
                      :foreground "orange"
                      :background (color-darken-name (face-background 'default) 5)
                      :box '(:color "dark slate gray" :line-width -1)
                      :weight 'normal
                      )
  (set-face-attribute 'anzu-replace-to nil
                      :foreground "yellow"
                      :background (color-darken-name (face-background 'default) 5)
                      :weight 'bold
                      )
  ;; anzu regexp matched groups
  (set-face-attribute 'anzu-match-1 nil
                      :foreground "white"
                      :background "dark red"
                      )
  (set-face-attribute 'anzu-match-2 nil
                      :foreground "white"
                      :background "dark green"
                      )
  (set-face-attribute 'anzu-match-3 nil
                      :foreground "#222222"
                      :background "tomato"
                      )

  (global-anzu-mode +1)
  ;; (anzu-mode +1)
  )


;;; [ Swpier ] -- gives you an overview as you search for a regex.

(use-package swiper
  :config
  (setq ivy-height 8)

  ;; current selection in ivy "in minibuffer"
  (set-face-attribute 'ivy-current-match nil
                      :foreground nil
                      :background "dark slate gray"
                      ;; :background (color-darken-name (face-background 'default) 5)
                      )

  ;; the matched line "in buffer"
  (set-face-attribute 'swiper-line-face nil
                      :foreground nil
                      :background (color-darken-name (face-background 'default) 5)
                      )
  (set-face-attribute 'swiper-match-face-1 nil
                      :foreground "white"
                      :background "dark red"
                      )
  (set-face-attribute 'swiper-match-face-2 nil
                      :foreground "white"
                      :background "dark green"
                      )
  (set-face-attribute 'swiper-match-face-3 nil
                      :foreground "#222222"
                      :background "tomato"
                      )
  (set-face-attribute 'swiper-match-face-4 nil
                      :foreground "black"
                      :background "sky blue"
                      )
  
  (define-key my-search-prefix (kbd "C-s") 'swiper)
  (define-key my-search-prefix (kbd "C-r") 'swiper)

  ;; if swiper is available, then replace `vr/isearch' with `swiper'.
  (if (functionp 'swiper)
      (progn
        (global-set-key (kbd "C-s") 'swiper)
        (global-set-key (kbd "C-r") 'swiper)))
  )


;;; [ swiper-helm ]

;; This package gives an overview of the current regex search
;; candidates.  The search regex can be split into groups with a
;; space.  Each group is highlighted with a different face.
;;
;; The overview back end is `helm'.
;;
;; It can double as a quick `regex-builder', although only single
;; lines will be matched.

;; 'helm, 'ivy.
;; FIXME: helm error.
;; (if (featurep 'helm)
;;     (setq swiper-completion-method 'helm)
;;   (setq swiper-completion-method 'ivy)
;;   )

;; (require 'swiper-helm)
;;
;; ;; (setq swiper-helm-display-function 'swiper-helm-default-display-buffer)
;;
;; (if (and (featurep 'swiper-helm) (featurep 'helm))
;;     (global-set-key (kbd "C-s") 'swiper-helm))


;;; [ Lazy Search ]

;; (require 'lazy-search)


;;; [ occur ]

(if (featurep 'helm)
    (define-key my-search-prefix (kbd "o") 'helm-occur)
  (define-key my-search-prefix (kbd "o") 'occur))


;;; [ multi-occur ]

(if (featurep 'helm)
    (define-key my-search-prefix (kbd "O") 'helm-multi-occur)
  (define-key my-search-prefix (kbd "O") 'multi-occur))

(define-key my-search-prefix (kbd "M-o") 'multi-occur-in-matching-buffers)

(define-key my-highlight-symbol-prefix (kbd "M-r") 'highlight-lines-matching-regexp)
(define-key my-search-prefix (kbd "M-h") 'how-many)


;;; [ replace+.el ]


;;; [ Grep ]


;;; [ helm-grep ]

;; (setq helm-grep-default-command "grep -a -d skip %e -n%cH -e %p %f")

(define-key my-search-prefix (kbd "g") 'grep)


;;; [ Ack ]

(require 'ack)


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

;; (require 'ack-and-a-half)
;;
;; ;; Create shorter aliases
;; (defalias 'ack 'ack-and-a-half)
;; (defalias 'ack-same 'ack-and-a-half-same)
;; (defalias 'ack-find-file 'ack-and-a-half-find-file)
;; (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)
;;
;; (setq ack-and-a-half-use-ido t               ; use ido to provide completions
;;       ;; ack-and-a-half-executable "ack-grep"
;;       ;; ack-and-a-half-arguments ; extra arguments passed to ack
;;       ack-and-a-half-ignore-case 'smart
;;       ack-and-a-half-regexp-search t
;;       ack-and-a-half-regexp-history t
;;       ack-and-a-half-use-environment t
;;       ;; (ack-and-a-half-same)
;;       ;; ack-and-a-half-mode-type-default-alist
;;       ack-and-a-half-mode-type-alist nil
;;       ack-and-a-half-literal-history t
;;       ;; ack-and-a-half-prompt-for-directory 'unless-guessed
;;       ;; ack-and-a-half-root-directory-functions '(ack-and-a-half-guess-project-root)
;;       ack-and-a-half-prompt-for-directory t
;;       )
;;
;; ;; add more project root file patterns.
;; ;; (add-to-list 'ack-and-a-half-project-root-file-patterns "\\.kk")
;;
;; (unless (boundp 'ack-map)
;;   (define-prefix-command 'ack-map))
;; (define-key my-search-prefix (kbd "k") 'ack-map)
;;
;; (define-key ack-map (kbd "k") 'ack)
;; (define-key ack-map (kbd "s") 'ack-same)
;; (define-key ack-map (kbd "f") 'ack-find-file)
;; (define-key ack-map (kbd "F") 'ack-find-file-same)


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
;;
;; - `ag-mode-hook' :: before search
;; - `ag-search-finished-hook' :: when finished search

(setq ag-highlight-search t
      ag-reuse-buffers 't
      ag-reuse-window nil ; nil, or 't. (I use value `nil' for popwin to capture)
      ;; ag-arguments
      )

(use-package ag
  :config
  (set-face-attribute 'ag-hit-face nil
                      :foreground "gray" :background "black")
  (set-face-attribute 'ag-match-face nil
                      :inverse-video nil
                      :foreground "red"
                      :background (color-darken-name (face-background 'default) 5)
                      )
  )

;; This will auto open search results in other window.
;; (add-hook 'ag-mode-hook #'next-error-follow-minor-mode) ; so you can navigate with 'n' & 'p'.

(unless (boundp 'ag-map)
  (define-prefix-command 'ag-map))
(define-key my-search-prefix (kbd "a") 'ag-map)

(define-key ag-map (kbd "a") 'ag)
(define-key ag-map (kbd "r") 'ag-regexp)
(define-key ag-map (kbd "p") 'ag-regexp-project-at-point) ; 'ag, 'ag-regexp,


;;; [ helm-ag ]

;;; Usage:
;;
;; - [C-c ?] :: show helm message.
;; - helm-ag :: Input search word with ag command. You can change search directory with C-u prefix.
;; - helm-ag-this-file :: Same as helm-ag except to search only current file
;; - helm-do-ag :: Search with ag like helm-do-grep.
;; - [C-l] :: search in parent directory.
;; - [C-c o] :: open other window.
;; - [C-c C-e] :: switch to ag edit mode.
;;   - [C-c C-c] :: commit changes.
;;   - [C-c C-k] :: abort.
;; - helm-ag-pop-stack :: Move to point before jump
;; - helm-ag-clear-stack :: Clear context stack
;; - Helm persistent action :: You can see file content temporarily by persistent action(C-z) at helm-ag and helm-ag-this-file.
;;   - [F3] / [C-x C-s] :: save ag results to buffer

(setq helm-ag-insert-at-point 'symbol ; same thing as `thing-at-point' such ash: 'word, symbol,
      helm-ag-base-command "ag --nocolor --nogroup --ignore-case" ; helm use color match, so use option `--nocolor' here.
      helm-ag-command-option "--all-text"
      helm-ag-source-type 'one-line ; 'one-line, 'file-line
      helm-ag-edit-save t ; save buffers you edit at editing completed.
      )

(define-key ag-map (kbd "a") 'helm-ag)


;;; [ helm-recoll ] -- full text search tool based on Xapian backend.

;;; Usage:
;;
;; -

;;; You need to create some helm-recoll sources before you can use them. You can
;;; create sources using the `helm-recoll-create-source' function, e.g. like
;;; this:
;; (helm-recoll-create-source "docs" "~/.recoll/docs")
;; (helm-recoll-create-source "progs" "~/.recoll/progs")

;;; Then you can use the sources in helm like this:
;; (helm :sources '(helm-source-recoll-docs helm-source-recoll-progs :buffer *helm recoll*))


;;; [ awk-it ] -- run AWK interactively on region!

;;; Usage:
;;
;; - `awk-it-' prefix
;; - [M-x awk-it]

;; (require 'awk-it)

;; (define-key my-search-prefix (kbd "w") 'awk-it)


;;; [ platinum searcher (pt) ]

;; (setq pt-arguments "--smart-case")

(unless (boundp 'pt-prefix)
  (define-prefix-command 'pt-prefix))
(define-key my-search-prefix (kbd "p") 'pt-prefix)

(define-key pt-prefix (kbd "p") 'pt-regexp)
(define-key pt-prefix (kbd "r") 'pt-regexp)
(define-key pt-prefix (kbd "f") 'pt-regexp-file-pattern)
(define-key pt-prefix (kbd "P") 'projectile-pt)


;;; [ helm-pt ]

;; (setq helm-pt-args "--smart-case"
;;       helm-pt-insert-at-point t
;;       )

(if (featurep 'helm-pt)
    (progn
      (define-key pt-prefix (kbd "h") 'helm-do-pt)
      (define-key pt-prefix (kbd "o") 'helm-projectile-pt)
      )
  )


(unless (boundp 'my-search-language-prefix)
  (define-prefix-command 'my-search-language-prefix))
(define-key my-search-prefix (kbd "l") 'my-search-language-prefix)


;;; [ pinyin-search ] --

(define-key my-search-language-prefix (kbd "c") 'pinyin-search)
(define-key my-search-language-prefix (kbd "C") 'pinyin-search-backward)


;;; [ migemo ] -- provides Japanese increment search with 'Romanization of Japanese'(ローマ字).

;; https://github.com/emacs-jp/migemo



(define-key my-search-prefix (kbd "s") 'ag-regexp)


(provide 'init-my-emacs-search)

;;; init-my-emacs-search.el ends here
