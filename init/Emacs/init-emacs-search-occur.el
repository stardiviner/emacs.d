;;; init-emacs-search-occur.el --- init for occur
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ occur ]

(use-package occur
  :no-require t
  :bind (:map search-prefix ("o" . occur))
  :init
  ;; Activate `occur' easily inside `isearch'.
  (define-key isearch-mode-map (kbd "M-o")
    (lambda () (interactive)
      (let ((case-fold-search isearch-case-fold-search))
        (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))
  
  (add-to-list 'display-buffer-alist '("^\\*Occur\\*" . (display-buffer-below-selected)))
  ;; make `occur-mode-goto-occurrence' open result target in current window.
  (add-to-list 'display-buffer-alist
               '((lambda (&rest _)
                   (eq this-command 'occur-mode-goto-occurrence))
                 (display-buffer-reuse-window display-buffer-same-window)
                 (inhibit-same-window . nil))))

;;; [ multi-occur ]

;; (use-package multi-occur
;;   :ensure t
;;   :defer t
;;   :init
;;   (define-key search-prefix (kbd "O") 'multi-occur)
;;   (define-key search-prefix (kbd "M-o") 'multi-occur-in-matching-buffers)
;;   (define-key search-prefix (kbd "M-h") 'how-many)
;;   )


(provide 'init-emacs-search-occur)

;;; init-emacs-search-occur.el ends here
