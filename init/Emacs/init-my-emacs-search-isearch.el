;;; init-my-emacs-search-isearch.el --- init for isearch
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'isearch-prefix)
  (define-prefix-command 'isearch-prefix))
(define-key my-search-prefix (kbd "i") 'isearch-prefix)


;;; [ Isearch ] -- Incremental Search

(use-package isearch
  :bind (("C-s" . isearch-forward)
         ("C-r" . isearch-backward)
         ("C-M-s" . isearch-forward-regexp)
         ("M-%" . query-replace-regexp)
         :map isearch-prefix
         ("i" . isearch-forward)
         ("C-i" . isearch-forward-symbol-at-point)
         ("I" . isearch-forward-regexp)
         ("f" . isearch-forward)
         ("C-f" . isearch-forward-symbol-at-point)
         ("F" . isearch-forward-regexp)
         ("b" . isearch-backward)
         ("B" . isearch-backward-regexp)
         ("o" . isearch-occur)
         ("r" . vr/isearch-forward)
         ("R" . vr/isearch-backward)
         )
  :config
  (setq search-highlight t
        query-replace-highlight t
        query-replace-show-replacement t
        ;; query-replace-from-to-separator "->"
        isearch-allow-scroll t
        isearch-lazy-highlight t
        )
  ;; case smart
  (setq-default case-fold-search t
                case-replace t
                )
  ;; default mode to use when starting isearch.
  (setq search-default-mode 'char-fold-to-regexp ; for isearch [C-s]
        ;; replace-char-fold t ; for command `query-replace'
        )

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
  )


;;;_ query & replace

(setq query-replace-highlight t
        query-replace-lazy-highlight t
        query-replace-show-replacement t
        query-replace-from-to-separator " → "
        )

;;; [ visual-regexp ] -- A regexp/replace command for Emacs with interactive visual feedback.

(use-package visual-regexp
  :ensure t
  :bind (("C-s" . vr/isearch-forward)
         ("C-r" . vr/isearch-backward)
         ("M-%" . vr/replace)
         :map my-search-prefix
         ("R" . vr/replace)
         ("Q" . vr/query-replace)
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
        anzu-deactivate-region nil
        anzu-use-migemo (and (featurep 'migemo) t)
        anzu-replace-to-string-separator " ⇨ "
        )
  )


;;; [ Swpier ] -- gives you an overview as you search for a regex.

(use-package swiper
  :ensure t
  :defer t
  :bind (("C-s" . swiper)
         ("C-c u" . swiper-all))
  )


(provide 'init-my-emacs-search-isearch)

;;; init-my-emacs-search-isearch.el ends here
