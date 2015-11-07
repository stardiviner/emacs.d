;;; init-my-emacs-search-isearch.el --- init for isearch
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


(setq search-highlight t
      query-replace-highlight t
      query-replace-show-replacement t
      ;; query-replace-from-to-separator "->"
      isearch-allow-scroll t
      )

;; (setq isearch-lazy-highlight-??)

;; isearch
(set-face-attribute 'isearch nil
                    :inherit nil
                    :inverse-video nil
                    :background (color-darken-name (face-background 'default) 5)
                    :foreground "pink"
                    :box '(:color "green" :line-width 1)
                    :slant 'italic
                    :weight 'normal)
(set-face-attribute 'isearch-fail nil
                    :inherit nil
                    :inverse-video nil
                    :background (color-darken-name (face-background 'default) 5)
                    :foreground "dark red"
                    :weight 'bold
                    :slant 'italic)
;; match
(set-face-attribute 'lazy-highlight nil
                    :inherit nil
                    :inverse-video nil
                    :background (color-darken-name (face-background 'default) 5)
                    :foreground "cyan"
                    :weight 'bold
                    )
(set-face-attribute 'match nil
                    :inherit nil
                    :inverse-video nil
                    :background (color-darken-name (face-background 'default) 3)
                    :foreground "red"
                    )
;; replace
(set-face-attribute 'query-replace nil
                    :inherit nil
                    :inverse-video nil
                    :background (color-darken-name (face-background 'default) 5)
                    :foreground "orange"
                    :weight 'bold
                    :box '(:color "black" :line-width 1 :style nil))


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


;;;_ query & replace

(setq query-replace-highlight t
      query-replace-lazy-highlight t
      query-replace-show-replacement t
      ;; TODO: added in Emacs 25.
      ;; query-replace-from-to-separator
      )


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
                      :background "dark slate gray"
                      ;; :background (color-darken-name (face-background 'default) 5)
                      )

  ;; the matched line "in buffer"
  (set-face-attribute 'swiper-line-face nil
                      :background (color-darken-name (face-background 'default) 5)
                      )
  (set-face-attribute 'swiper-match-face-1 nil
                      :foreground "white"
                      :background "dark slate blue"
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
  (set-face-attribute 'swiper-minibuffer-match-face-1 nil
                      :inherit 'swiper-match-face-1
                      )
  (set-face-attribute 'swiper-minibuffer-match-face-2 nil
                      :inherit 'swiper-match-face-2
                      )
  (set-face-attribute 'swiper-minibuffer-match-face-3 nil
                      :inherit 'swiper-match-face-3
                      )
  (set-face-attribute 'swiper-minibuffer-match-face-4 nil
                      :inherit 'swiper-match-face-4
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




(provide 'init-my-emacs-search-isearch)

;;; init-my-emacs-search-isearch.el ends here
