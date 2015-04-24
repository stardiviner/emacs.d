;;; init-my-emacs-bookmark.el ---
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;; [ Bookmark ]
;;; Emacs built-in bookmark

;;; Usage
;;; - [C-x r m] :: mark bookmark.
;;; - [C-x r b] :: jump to bookmark.
;;; - [C-x r l] :: list bookmarks.
;;   - a -- show annotation for current bookmark
;;   - A -- show all annotations
;;   - d -- mark as delete
;;   - e -- edit the annotation for current bookmark
;;   - m -- mark various entries for display and other operations
;;   - o -- visit the current bookmark in another window, keeping the bookmark list open
;;   - C-o -- switch to the current bookmark in another window
;;   - r -- rename the current bookmark
;;   - x -- execute marked status actions
;; - [M-x bookmark-set] -- add current page into bookmark

;; (require 'bookmark)
(setq-default bookmark-default-file (expand-file-name "bookmarks.el" user-emacs-directory))

(setq bookmark-save-flag 1)

(unless (boundp 'my-bookmark-map)
  (define-prefix-command 'my-bookmark-map))
(global-set-key (kbd "C-x r b") 'my-bookmark-map)

(global-unset-key (kbd "C-x r l"))
(global-unset-key (kbd "C-x r m"))

(define-key my-bookmark-map (kbd "b") 'bookmark-jump)
(define-key my-bookmark-map (kbd "j") 'bookmark-jump)
(define-key my-bookmark-map (kbd "l") 'bookmark-bmenu-list)
(define-key my-bookmark-map (kbd "a") 'bookmark-set)
(define-key my-bookmark-map (kbd "M") 'bookmark-set)




;;; [ bm.el ]

(require 'bm)
;; or
;; (autoload 'bm-toggle   "bm" "Toggle bookmark in current buffer." t)
;; (autoload 'bm-next     "bm" "Goto bookmark."                     t)
;; (autoload 'bm-previous "bm" "Goto previous bookmark."            t)

;; (global-set-key (kbd "<left-fringe> <mouse-5>") 'bm-next-mouse)
;; (global-set-key (kbd "<left-fringe> <mouse-4>") 'bm-previous-mouse)
(global-set-key (kbd "<left-margin> <mouse-1>") 'bm-toggle-mouse)

;;; extend upper Emacs built-in bookmark mechanism.
(define-key my-bookmark-map (kbd "m") 'bm-toggle)

;;; the markers on the right fringe instead of the left
;; (setq bm-marker 'bm-marker-right)
;;; cycle bookmark in LIFO order
(setq bm-in-lifo-order t)
;;; cycle through bookmarks in all open buffers
(setq bm-cycle-all-buffers t)


(provide 'init-my-emacs-bookmark)

;;; init-my-emacs-bookmark.el ends here
