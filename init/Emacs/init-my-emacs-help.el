;;; init-my-emacs-help.el --- init Emacs's help settings.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Info ]

(use-package info
  :bind ("C-h i" . info-display-manual)
  :init
  ;; - `Info-default-directory-list'
  (add-to-list 'Info-directory-list
               (expand-file-name "info/" user-emacs-directory))
  (add-to-list 'display-buffer-alist
               '("\\*info\\*" . (display-buffer-below-selected)))
  ;; let `[s]' search like =isearch= for incremental search.
  (setq Info-isearch-search t)
  ;; - `Info-mode-hook'
  )


;;; [ info+ ]

(use-package info+
  :ensure t
  :defer t
  :config
  (setq Info-breadcrumbs-in-header-flag t
        ;; Info-display-node-header-fn
        Info-fit-frame-flag t
        Info-fontify-angle-bracketed-flag t
        Info-fontify-quotations-flag t
        Info-fontify-reference-items-flag t
        Info-fontify-single-quote-flag t
        Info-saved-nodes t))


;; (use-package niceify-info
;;   :ensure t
;;   :init
;;   (add-hook 'Info-selection-hook #'niceify-info))

;;; [ sicp ] -- Structure and Interpretation of Computer Programs in info format.

(use-package sicp
  :ensure t)


(add-to-list 'display-buffer-alist
             '("^\\*Help\\*$" (display-buffer-below-selected)))
(add-to-list 'display-buffer-alist
             '("^\\*Apropos\\*$" (display-buffer-below-selected)))

;;; [ helpful ] -- A better Emacs *help* buffer.

;; (use-package helpful
;;   :ensure t
;;   :bind (("C-h f" . helpful-callable) ; replace Emacs default keybindings.
;;          ("C-h v" . helpful-variable)
;;          ("C-h k" . helpful-key)
;;          ;; convenient keybinding:
;;          ("C-h C-." . helpful-at-point))
;;   :config
;;   (add-to-list 'display-buffer-alist
;;                '("^\\*helpful.*$" (display-buffer-below-selected)))
;;   )


(provide 'init-my-emacs-help)

;;; init-my-emacs-help.el ends here
