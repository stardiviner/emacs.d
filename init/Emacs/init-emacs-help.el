;;; init-emacs-help.el --- init Emacs's help settings.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


;;; NOTE: disable split window performance slow for `company-box-doc'.
(add-to-list 'display-buffer-alist '("^\\*Help\\*$" . (display-buffer-below-selected)))
(add-to-list 'display-buffer-alist '("^\\*Apropos\\*$" . (display-buffer-below-selected)))

;;; open source code link file on Help buffer in current window.
(add-to-list 'display-buffer-alist
             '((lambda (&rest _) (memq this-command '(push-button))) . ((display-buffer-same-window) (inhibit-same-window . nil))))

;;; [ Info ]

(use-package info
  :bind (("C-h i" . info-display-manual)
         :map Info-mode-map ("M-g n" . Info-search-next))
  :custom (Info-isearch-search t) ; let `[s]' search like =isearch= for incremental search.
  :init (add-to-list 'display-buffer-alist '("\\*info\\*" . (display-buffer-below-selected)))
  ;; :config
  ;; - `Info-default-directory-list'
  ;; (add-to-list 'Info-directory-list
  ;;              (expand-file-name "info/" user-emacs-directory))
  )

;;; [ info-rename-buffer ] -- Rename Info buffers to match manuals.

(use-package info-rename-buffer
  :ensure t
  :defer t
  :init (info-rename-buffer-mode))

;;; [ info-colors ] -- A modern adaption of the extra coloring provided by info+ package.

(use-package info-colors
  :ensure t
  :defer t
  :init (add-hook 'Info-selection-hook 'info-colors-fontify-node))


(provide 'init-emacs-help)

;;; init-emacs-help.el ends here
