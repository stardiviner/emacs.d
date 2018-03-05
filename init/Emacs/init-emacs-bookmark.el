;;; init-emacs-bookmark.el --- init for Emacs bookmarks.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'bookmark-prefix)
  (define-prefix-command 'bookmark-prefix))
(global-set-key (kbd "C-x r b") 'bookmark-prefix)


;; [ Bookmark ] -- Emacs built-in bookmark

(require 'bookmark)

;; load bookmarks from file.
(with-eval-after-load 'bookmark
  (setq-default bookmark-default-file
                (expand-file-name "bookmarks.el" user-emacs-directory))
  (bookmark-maybe-load-default-file))

(setq bookmark-save-flag 1
      bookmark-automatically-show-annotations t)

(global-unset-key (kbd "C-x r l"))
(global-unset-key (kbd "C-x r m"))

(define-key bookmark-prefix (kbd "b") 'bookmark-jump)
(define-key bookmark-prefix (kbd "j") 'bookmark-jump)
(define-key bookmark-prefix (kbd "l") 'bookmark-bmenu-list)
(define-key bookmark-prefix (kbd "m") 'bookmark-set)
(define-key bookmark-prefix (kbd "a") 'bookmark-set)


;;; [ bm.el ] -- Visual Bookmarks for GNU Emacs.

(use-package bm
  :ensure t
  :defer t
  :init
  ;; mouse
  (global-set-key (kbd "<left-fringe> <mouse-5>") 'bm-next-mouse)
  (global-set-key (kbd "<left-fringe> <mouse-4>") 'bm-previous-mouse)
  (global-set-key (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)
  (global-set-key (kbd "<left-margin> <mouse-1>") 'bm-toggle-mouse)

  ;; (global-set-key (kbd "C-c =") 'bm-toggle)
  ;; (global-set-key (kbd "C-c ]") 'bm-next)
  ;; (global-set-key (kbd "C-c [") 'bm-previous)
  
  (unless (boundp 'bookmark-bm-prefix)
    (define-prefix-command 'bookmark-bm-prefix))
  (global-set-key (kbd "M-g b") 'bookmark-bm-prefix)

  ;; mark
  (define-key bookmark-bm-prefix (kbd "b") 'bm-toggle)
  ;; navigate
  (define-key bookmark-bm-prefix (kbd "n") 'bm-next)
  (define-key bookmark-bm-prefix (kbd "p") 'bm-previous)
  ;; show
  (define-key bookmark-bm-prefix (kbd "j") 'bm-show)
  (define-key bookmark-bm-prefix (kbd "l") 'bm-show-all)
  (define-key bookmark-bm-prefix (kbd "N") 'bm-show-next)
  (define-key bookmark-bm-prefix (kbd "P") 'bm-show-prev)
  ;; persistent
  (define-key bookmark-bm-prefix (kbd "t") 'bm-toggle-buffer-persistence)
  ;; save to repository
  (define-key bookmark-bm-prefix (kbd "s") 'bm-save) ; Save bookmarks to persistent repository.
  (define-key bookmark-bm-prefix (kbd "C-s") 'bm-buffer-save) ; Save all bookmarks to repository.
  (define-key bookmark-bm-prefix (kbd "M-s") 'bm-buffer-save-all) ; Save bookmarks in all buffers.
  ;; remove/delete
  (define-key bookmark-bm-prefix (kbd "c") 'bm-remove-all-current-buffer)
  (define-key bookmark-bm-prefix (kbd "M-c") 'bm-remove-all-all-buffers)
  
  :config
  (setq bm-in-lifo-order t)
  (setq bm-cycle-all-buffers nil)
  (setq temporary-bookmark-p nil)
  (setq bm-annotate-on-create t)
  (setq bm-highlight-style 'bm-highlight-line-and-fringe)
  (setq bm-marker 'bm-marker-left)

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
  
  (setq bm-restore-repository-on-load t)
  ;; this could affect Emacs increasing running usage Persistence
  (setq bm-repository-file "~/.emacs.d/.bm-repository"
        bm-repository-size 100)
  )


(provide 'init-emacs-bookmark)

;;; init-emacs-bookmark.el ends here