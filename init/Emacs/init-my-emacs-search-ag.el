;;; init-my-emacs-search-ag.el --- init for ag
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

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



(provide 'init-my-emacs-search-ag)

;;; init-my-emacs-search-ag.el ends here
