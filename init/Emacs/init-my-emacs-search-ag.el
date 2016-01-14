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

(use-package ag
  :commands ag
  :config
  (setq ag-highlight-search t
        ag-reuse-buffers 't
        ag-reuse-window nil ; nil, or 't. (I use value `nil' for popwin to capture)
        ;; ag-arguments
        )
  
  (set-face-attribute 'ag-hit-face nil
                      :foreground "gray" :background "black")
  (set-face-attribute 'ag-match-face nil
                      :inverse-video nil
                      :foreground "red"
                      :background (color-darken-name (face-background 'default) 5)
                      )
  
  ;; This will auto open search results in other window.
  ;; (add-hook 'ag-mode-hook #'next-error-follow-minor-mode) ; so you can navigate with 'n' & 'p'.

  (unless (boundp 'ag-map)
    (define-prefix-command 'ag-map))
  (define-key my-search-prefix (kbd "a") 'ag-map)

  (define-key ag-map (kbd "a") 'ag)
  (define-key ag-map (kbd "r") 'ag-regexp)
  (define-key ag-map (kbd "p") 'ag-regexp-project-at-point) ; 'ag, 'ag-regexp,
  )


(provide 'init-my-emacs-search-ag)

;;; init-my-emacs-search-ag.el ends here
