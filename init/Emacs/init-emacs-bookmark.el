;;; init-emacs-bookmark.el --- init for Emacs bookmarks.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'bookmark-prefix)
  (define-prefix-command 'bookmark-prefix))
(global-set-key (kbd "C-x r b") 'bookmark-prefix)


;; [ Bookmark ] -- Emacs built-in bookmark

(use-package bookmark
  :defer t
  :commands (bookmark-maybe-load-default-file)
  :init
  (global-unset-key (kbd "C-x r l"))
  (global-unset-key (kbd "C-x r m"))
  :bind (:map bookmark-prefix
              ("b" . bookmark-jump)
              ("j" . bookmark-jump)
              ("l" . bookmark-bmenu-list)
              ("a" . bookmark-set))
  :config
  (setq-default bookmark-default-file
                (expand-file-name "bookmarks.el" user-emacs-directory))
  (bookmark-maybe-load-default-file))

;;; [ bm.el ] -- Visual Bookmarks for GNU Emacs.

(use-package bm
  :ensure t
  :defer t
  :preface (setq bm-face 'highlight)
  (unless (boundp 'bookmark-bm-prefix)
    (define-prefix-command 'bookmark-bm-prefix))
  (global-set-key (kbd "M-g b") 'bookmark-bm-prefix)
  :bind (:map bookmark-bm-prefix
              ;; mark
              ("b" . bm-toggle)
              ;; navigate
              ("n" . bm-next)
              ("p" . bm-previous)
              ;; show
              ("l" . bm-show)
              ("L" . bm-show-all)
              ("N" . bm-show-next)
              ("P" . bm-show-prev)
              ;; persistent
              ("t" . bm-toggle-buffer-persistence)
              ;; save to repository
              ("s" . bm-save) ; Save bookmarks to persistent repository.
              ;; ("C-s" . bm-buffer-save) ; Save all bookmarks to repository.
              ("M-s" . bm-buffer-save-all) ; Save bookmarks in all buffers.
              ;; remove/delete
              ("c" . bm-remove-all-current-buffer)
              ("M-c" . bm-remove-all-all-buffers))
  :init (add-to-list 'display-buffer-alist
                     '("\\*bm-bookmarks\\*" (display-buffer-same-window)))
  (setq bm-in-lifo-order t
        bm-cycle-all-buffers nil
        bm-annotate-on-create t
        bm-highlight-style 'bm-highlight-line-and-fringe
        ;; store & restore bm session.
        bm-restore-repository-on-load t
        ;; this could affect Emacs increasing running usage Persistence
        bm-repository-file (expand-file-name ".bm-repository" user-emacs-directory)
        bm-repository-size 100))

(require 'counsel-bm)
(define-key 'bookmark-bm-prefix (kbd "TAB") 'counsel-bm)
(define-key 'bookmark-bm-prefix (kbd "j") 'counsel-bm)
(define-key 'bookmark-bm-prefix (kbd "C-s") 'counsel-bm-from-isearch)


(provide 'init-emacs-bookmark)

;;; init-emacs-bookmark.el ends here
