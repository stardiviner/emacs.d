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
               '("\\*info\\*" . (display-buffer-reuse-window display-buffer-below-selected)))
  ;; let `[s]' search like =isearch= for incremental search.
  (setq Info-isearch-search t)
  ;; - `Info-mode-hook'
  )

;;; [ info-colors ] -- A modern adaption of the extra coloring provided by info+ package.

(use-package info-colors
  :ensure t
  :defer t
  :init
  (add-hook 'Info-selection-hook 'info-colors-fontify-node))

;;; [ sicp ] -- Structure and Interpretation of Computer Programs in info format.

(use-package sicp
  :ensure t
  :defer t)


(add-to-list 'display-buffer-alist
             '("^\\*Help\\*$" (display-buffer-reuse-window display-buffer-below-selected)))
(add-to-list 'display-buffer-alist
             '("^\\*Apropos\\*$" (display-buffer-reuse-window display-buffer-below-selected)))

;;; [ helpful ] -- A better Emacs *help* buffer.

;; (use-package helpful
;;   :ensure t
;;   :defer t
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
