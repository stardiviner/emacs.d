;;; init-my-emacs-bookmark.el ---
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; Emacs built-in bookmark

;;; Usage
;;; - [C-x r m] :: mark bookmark.
;;; - [C-x r b] :: jump to bookmark.
;;; - [C-x r l] :: list bookmarks.

(setq-default bookmark-default-file (expand-file-name "bookmarks.el" user-emacs-directory))


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
(global-set-key (kbd "C-x r h") 'bm-toggle)

;;; the markers on the right fringe instead of the left
;; (setq bm-marker 'bm-marker-right)
;;; cycle bookmark in LIFO order
(setq bm-in-lifo-order t)
;;; cycle through bookmarks in all open buffers
(setq bm-cycle-all-buffers t)


(provide 'init-my-emacs-bookmark)

;;; init-my-emacs-bookmark.el ends here
