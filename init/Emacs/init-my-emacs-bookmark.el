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



(provide 'init-my-emacs-bookmark)

;;; init-my-emacs-bookmark.el ends here
