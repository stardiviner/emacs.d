;;; init-my-emacs-navigation.el --- init Emacs Navigation.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Mark ] --- [C-SPC / C-@] + [C-u C-SPC / C-u C-@] + [C-`] / [M-`]

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region.
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(global-set-key (kbd "C-`") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(global-set-key (kbd "M-`") 'jump-to-mark)

;; (defun exchange-point-and-mark-no-activate ()
;;   "Identical to \\[exchange-point-and-mark] but will not activate the region."
;;   (interactive)
;;   (exchange-point-and-mark)
;;   (deactivate-mark nil))
;; (define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)


;;;_ show-marks

(global-set-key (kbd "C-c `") 'show-marks)


;;;_ visual-mark



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

;; FIXME: `my-bookmark-map' is not void yet. it is in init-my-emacs-bookmark.el
(unless (boundp 'my-bookmark-map)
  (define-prefix-command 'my-bookmark-map))

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

(use-package bm
  :config
  (set-face-attribute 'bm-face nil
                      :background "saddle brown")
  (set-face-attribute 'bm-fringe-face nil
                      :inherit 'bm-face
                      :foreground "white"
                      :weight 'normal
                      )
  (set-face-attribute 'bm-persistent-face nil
                      :background "royal blue")
  (set-face-attribute 'bm-fringe-persistent-face nil
                      :inherit 'bm-persistent-face
                      :foreground "dark red"
                      :weight 'bold
                      )
  )


;;; [ ace-jump-mode -- Ace Jump Mode ]

;;; Usage:
;; "C-;" ==> ace-jump-word-mode
;;     enter first character of a word, select the highlighted key to move to it.
;; "C-'" ==> ace-jump-mode-pop-mark
;;     popup the mark to jump back.
;; "C-u C-c SPC" ==> ace-jump-char-mode
;;     enter a character for query, select the highlighted key to move to it.
;; "C-u C-u C-c SPC" ==> ace-jump-line-mode
;;     each non-empty line will be marked, select the highlighted key to move to it.

;; enable a more powerful jump back function from ace jump mode
(autoload 'ace-jump-mode-pop-mark "ace-jump-mode" "Ace jump back:-)" t)
(eval-after-load "ace-jump-mode" '(ace-jump-mode-enable-mark-sync))

;; (global-set-key [remap flyspell-auto-correct-previous-word] nil)
;; FIXME: this does not work, conflict with `flyspell-auto-correct-previous-word'.
(global-set-key (kbd "C-'") 'ace-jump-mode)
;; (define-key global-map (kbd "C-'") 'ace-jump-mode-pop-mark)


;;; [ ace-isearch ] -- A seamless bridge between isearch and ace-jump-mode.

;;; ace-isearch.el provides a minor mode which combines isearch and ace-jump-mode.

;;; Usage:
;; The "default" behavior can be summrized as:

;; *Length*
;; - L = 1 : ace-jump-mode
;; - 1 < L < 6 : isearch
;; - L >= 6 : helm-swoop-from-isearch

;; where L is the input string length during isearch. When L is 1, after a few
;; seconds specified by ace-isearch-input-idle-delay, ace-jump-mode will be
;; invoked. Of course you can customize the above behaviour.

;; (require 'ace-isearch)
;;
;; (setq ace-isearch-use-ace-jump t
;;       ;; ace-isearch-input-idle-delay 0.5
;;       ;; ace-isearch-input-length 6
;;       ;; ace-isearch-function-from-isearch 'helm-swoop-from-isearch ; 'swoop-from-isearch
;;       ;; ace-isearch-use-function-from-isearch t
;;       ;; ace-isearch-set-ace-jump-after-isearch-exit t
;;       ;; ace-isearch-use-fallback-function 
;;       )
;;
;; ;; (define-key swoop-map (kbd "C-s") 'swoop-action-goto-line-next)
;; ;; (define-key swoop-map (kbd "C-r") 'swoop-action-goto-line-prev)
;;
;; ;; (ace-isearch-mode +1)
;; (global-ace-isearch-mode +1)


;;; [ pophint ]

;; (require 'pophint)

;; (define-key global-map (kbd "C-;") 'pophint:do-flexibly)
;; (define-key global-map (kbd "C-+") 'pophint:do)
;; (define-key global-map (kbd "M-;") 'pophint:redo)
;; (define-key global-map (kbd "C-M-;") 'pophint:do-interactively)


;; recenter

;; (setq recenter-positions '(top middle bottom)) ; default '(middle top bottom)
;;
;; (global-set-key (kbd "C-l") 'recenter-top-bottom)


(provide 'init-my-emacs-navigation)

;;; init-my-emacs-navigation.el ends here
