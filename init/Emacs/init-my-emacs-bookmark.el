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




;;; [ bm.el ]

;;; Usage:
;;
;; - `bm-toggle' :: toggle bookmark.
;; - `bm-show' :: show bookmarks list for *current buffer*.
;; - `bm-show-all' :: show bookmarks list for *all buffer*.
;; - `bm-next/previous' :: navigate next/previous.
;; - `bm-cycle-all-buffers' :: cycle in all buffers bookmarks.
;; - `bm-bookmark-regexp'-[region] :: set bookmark based on regexp.
;; - `bm-bookmark-line' :: set bookmark based on line number.
;; - `bm-goto-position' :: goto line position or start of line.
;; - `bm-toggle-buffer-persistence' :: persistent bookmarks.
;; - `bm-remove-all-current-buffer' :: remove all bookmarks in *current buffer.
;; - `bm-remove-all-all-buffer' :: remove all bookmarks in *all buffer.
;; - `bm-bookmark-annotate' :: annotate bookmark (displayed in message area when navigating)

;; NOTE set this variable before load `bm.el' for repository persistence.
(setq bm-restore-repository-on-load t)

;;; the markers on the right fringe instead of the left
;; (setq bm-marker 'bm-marker-right)
;;; cycle bookmark in LIFO order
(setq bm-in-lifo-order t)
;;; cycle through bookmarks in all open buffers
(setq bm-cycle-all-buffers t)
;; prompt for bookmark annotation when create/add
(setq bm-annotate-on-create t)
;; bookmark styles:
(setq bm-highlight-style 'bm-highlight-line-and-fringe)
(setq bm-marker 'bm-marker-left)


;; TODO: this could affect Emacs increasing running usage
;; ;; Persistence
;; (setq bm-repository-file "~/.emacs.d/.bm-repository"
;;       bm-repository-size 100)
;; ;; loading the repository from file when on start up.
;; (add-hook 'after-init-hook 'bm-repository-load)
;; ;; Restoring bookmarks when on file find.
;; (add-hook 'find-file-hooks 'bm-buffer-restore)
;; ;; Saving bookmark data on killing a buffer
;; (add-hook 'kill-buffer-hook 'bm-buffer-save)
;; ;; Saving the repository to file when on exit.
;; ;; kill-buffer-hook is not called when Emacs is killed, so we
;; ;; must save all bookmarks first.
;; (add-hook 'kill-emacs-hook '(lambda nil
;;                               (bm-buffer-save-all)
;;                               (bm-repository-save)))
;; ;; Update bookmark repository when saving the file.
;; (add-hook 'after-save-hook 'bm-buffer-save)
;; ;; Restore bookmarks when buffer is reverted.
;; (add-hook 'after-revert-hook 'bm-buffer-restore)


;; (global-set-key (kbd "<left-fringe> <mouse-5>") 'bm-next-mouse)
;; (global-set-key (kbd "<left-fringe> <mouse-4>") 'bm-previous-mouse)
(global-set-key (kbd "<left-margin> <mouse-1>") 'bm-toggle-mouse)

(unless (boundp 'my-bookmark-bm-map)
  (define-prefix-command 'my-bookmark-bm-map))
(define-key my-bookmark-map (kbd "m") 'my-bookmark-bm-map)

;; mark
(define-key my-bookmark-bm-map (kbd "m") 'bm-toggle)
;; navigate
(define-key my-bookmark-bm-map (kbd "n") 'bm-next)
(define-key my-bookmark-bm-map (kbd "p") 'bm-previous)
;; show
(define-key my-bookmark-bm-map (kbd "j") 'bm-show)
(define-key my-bookmark-bm-map (kbd "J") 'bm-show-all)
(define-key my-bookmark-bm-map (kbd "N") 'bm-show-next)
(define-key my-bookmark-bm-map (kbd "P") 'bm-show-prev)
;; persistent
(define-key my-bookmark-bm-map (kbd "t") 'bm-toggle-buffer-persistence)
;; save to repository
(define-key my-bookmark-bm-map (kbd "s") 'bm-save) ; Save bookmarks to persistent repository.
(define-key my-bookmark-bm-map (kbd "b") 'bm-buffer-save) ; Save all bookmarks to repository.
(define-key my-bookmark-bm-map (kbd "B") 'bm-buffer-save-all) ; Save bookmarks in all buffers.
;; remove/delete
(define-key my-bookmark-bm-map (kbd "d") 'bm-remove-all-current-buffer)
(define-key my-bookmark-bm-map (kbd "D") 'bm-remove-all-all-buffers)

(set-face-attribute 'bm-face nil
                    :foreground nil :background "saddle brown")
(set-face-attribute 'bm-fringe-face nil
                    :inherit 'bm-face
                    :foreground "white"
                    :weight 'normal
                    )
(set-face-attribute 'bm-persistent-face nil
                    :foreground nil :background "royal blue")
(set-face-attribute 'bm-fringe-persistent-face nil
                    :inherit 'bm-persistent-face
                    :foreground "dark red"
                    :weight 'bold
                    )



(provide 'init-my-emacs-bookmark)

;;; init-my-emacs-bookmark.el ends here
