;;; init-my-emacs-search-isearch.el --- init for isearch
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Isearch ] -- Incremental Search

(setq search-highlight t
      query-replace-highlight t
      query-replace-show-replacement t
      ;; query-replace-from-to-separator "->"
      isearch-allow-scroll t
      )

(setq isearch-lazy-highlight t)

;; case smart
(setq-default case-fold-search t
              case-replace t
              )

;; default mode to use when starting isearch.
(setq search-default-mode 'char-fold-to-regexp ; for isearch [C-s]
      ;; replace-char-fold t ; for command `query-replace'
      )

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
(define-key my-isearch-prefix (kbd "C-i") 'isearch-forward-symbol-at-point)
(define-key my-isearch-prefix (kbd "I") 'isearch-forward-regexp)
(define-key my-isearch-prefix (kbd "f") 'isearch-forward)
(define-key my-isearch-prefix (kbd "C-f") 'isearch-forward-symbol-at-point)
(define-key my-isearch-prefix (kbd "F") 'isearch-forward-regexp)
(define-key my-isearch-prefix (kbd "b") 'isearch-backward)
(define-key my-isearch-prefix (kbd "B") 'isearch-backward-regexp)
(define-key my-isearch-prefix (kbd "o") 'isearch-occur)
(define-key my-isearch-prefix (kbd "r") 'vr/isearch-forward)
(define-key my-isearch-prefix (kbd "R") 'vr/isearch-backward)


;;; custom function

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
      ;; query-replace-from-to-separator
      )


;;; [ visual-regexp ] -- A regexp/replace command for Emacs with interactive visual feedback.

(use-package visual-regexp
  :ensure t
  :bind (("C-s" . vr/isearch-forward)
         ("C-r" . vr/isearch-backward)
         ("M-%" . vr/replace)
         :map my-search-prefix
         ("r" . vr/replace)
         ("q" . vr/query-replace)
         )
  :init
  ;; if you use multiple-cursors interface, this is for you:
  (if (featurep 'multiple-cursors)
      (define-key my-search-prefix (kbd "m") 'vr/mc-mark))
  ;; `vr/select-mc-mark', `vr/select-replace' etc.
  )

;; [ visual-regexp-steroids.el ] -- Extends visual-regexp to support other regexp engines.

(use-package visual-regexp-steroids
  :ensure t)


;;; [ anzu ] -- Emacs Port of anzu.vim.

(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace-regexp) ; anzu-query-replace
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode +1)
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
  )


;;; [ Swpier ] -- gives you an overview as you search for a regex.

(use-package swiper
  :ensure t
  :defer t
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         :map my-search-prefix
         ("C-s" . swiper)
         ("C-r" . swiper))
  )


(provide 'init-my-emacs-search-isearch)

;;; init-my-emacs-search-isearch.el ends here
