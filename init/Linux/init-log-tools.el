;;; init-log-tools.el --- init for Log tools.

;;; Commentary:



;;; Code:

;;; [ logview ] -- Emacs major mode for viewing log files.

(use-package logview
  :ensure t
  :mode (("\\.log\\'" . logview-mode)))

;;; [ view-mode ]

(use-package view
  :ensure t
  :defer t
  :init (add-hook 'logview-mode-hook #'view-mode)
  :config
  (defun View-goto-line-last (&optional line)
    "goto last line"
    (interactive "P")
    (goto-line (line-number-at-pos (point-max))))

  (define-key view-mode-map (kbd "e") 'View-scroll-half-page-forward)
  (define-key view-mode-map (kbd "u") 'View-scroll-half-page-backward)

  ;; less like
  (define-key view-mode-map (kbd "N") 'View-search-last-regexp-backward)
  (define-key view-mode-map (kbd "?") 'View-search-regexp-backward?)
  (define-key view-mode-map (kbd "g") 'View-goto-line)
  (define-key view-mode-map (kbd "G") 'View-goto-line-last)
  ;; Vim like
  (define-key view-mode-map (kbd "h") 'backward-char)
  (define-key view-mode-map (kbd "j") 'next-line)
  (define-key view-mode-map (kbd "k") 'previous-line)
  (define-key view-mode-map (kbd "l") 'forward-char))

;;; [ lognav-mode ] -- Navigate Log Error Messages with [M-n/p].

(use-package lognav-mode
  :ensure t
  :defer t
  :init (add-hook 'logview-mode-hook #'lognav-mode))

;;; [ log4j-mode ] -- Java applications' common log format Log4j.



(provide 'init-log-tools)

;;; init-log-tools.el ends here
