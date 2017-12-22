;;; init-my-prog-document-info.el --- init for Info
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Info ]

(global-set-key (kbd "C-h i") 'info-display-manual)

(add-to-list 'display-buffer-alist
             '("\\*info\\*" . (display-buffer-below-selected)))


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
        Info-saved-nodes t
        )
  )

;; (use-package niceify-info
;;   :ensure t
;;   :init
;;   (add-hook 'Info-selection-hook #'niceify-info))

;;; [ sicp ] -- Structure and Interpretation of Computer Programs in info format.

(use-package sicp
  :ensure t)



(provide 'init-my-prog-document-info)

;;; init-my-prog-document-info.el ends here
