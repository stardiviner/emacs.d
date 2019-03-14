;;; init-emacs-search-isearch.el --- init for isearch
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'isearch-prefix)
  (define-prefix-command 'isearch-prefix))
(define-key search-prefix (kbd "i") 'isearch-prefix)


;;; [ Isearch ] -- Incremental Search

(use-package isearch
  :defer t
  :bind (:map isearch-prefix
              ("i" . isearch-forward)
              ("f" . isearch-forward)
              ("SPC" . isearch-forward-symbol-at-point)
              ("r" . isearch-forward-regexp)
              ("b" . isearch-backward)
              ("R" . isearch-backward-regexp)
              ("o" . isearch-occur))
  :config
  (setq isearch-allow-scroll t)
  ;; case smart
  (setq-default case-fold-search t
                case-replace t)
  ;; default mode to use when starting isearch.
  (setq search-default-mode 'char-fold-to-regexp ; if you deal with multi-lingual stuff.
        ;; replace-char-fold t ; for command `query-replace'
        )
  )

;;; [ visual-regexp ] -- A regexp/replace command for Emacs with interactive visual feedback.

(use-package visual-regexp
  :ensure t
  :defer t
  :bind (("C-s" . vr/isearch-forward)
         ("C-r" . vr/isearch-backward)
         ("M-%" . vr/query-replace)
         :map search-prefix
         ("s" . vr/isearch-forward)
         ("M-s" . vr/isearch-backward)
         ("r" . vr/query-replace)
         ("R" . vr/replace))
  :init
  ;; if you use `multiple-cursors' interface, this is for you:
  (with-eval-after-load 'multiple-cursors
    (define-key search-prefix (kbd "m") 'vr/mc-mark))
  ;; `vr/select-mc-mark', `vr/select-replace' etc.
  )

;; [ visual-regexp-steroids.el ] -- Extends visual-regexp to support other regexp engines.

(use-package visual-regexp-steroids
  :ensure t
  :defer t)


;;; [ anzu ] -- Emacs Port of anzu.vim.

(use-package anzu
  :ensure t
  :defer t
  :bind (("M-%" . anzu-query-replace-regexp) ; anzu-query-replace
         ("C-M-%" . anzu-query-replace-regexp))
  :init
  (setq anzu-regexp-search-commands '(vr/isearch-forward
                                      vr/isearch-backward
                                      isearch-forward-regexp
                                      isearch-backward-regexp)
        anzu-cons-mode-line-p nil
        anzu-deactivate-region nil
        anzu-use-migemo (and (featurep 'migemo) t)
        anzu-replace-to-string-separator " ⇨ "
        )
  :config
  (global-anzu-mode 1))

;;; [ Swpier ] -- gives you an overview as you search for a regex.

;; (use-package swiper
;;   :ensure t
;;   :defer t
;;   ;; :bind ("C-s" . swiper)
;;   :bind ("C-s" . counsel-grep-or-swiper)
;;   :init (setq counsel-grep-base-command "grep -E -i -n -e %s %s")
;;   :config (set-face-attribute 'swiper-line-face nil
;;                               :inherit nil
;;                               :foreground nil))


(provide 'init-emacs-search-isearch)

;;; init-emacs-search-isearch.el ends here
