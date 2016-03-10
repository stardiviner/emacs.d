;;; init-my-emacs-search-occur.el --- init for occur
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ occur ]

(define-key my-search-prefix (kbd "o") 'occur)


;;; [ multi-occur ]

(use-package multi-occur
  :config
  (define-key my-search-prefix (kbd "O") 'multi-occur)
  (define-key my-search-prefix (kbd "M-o") 'multi-occur-in-matching-buffers)
  (define-key my-search-prefix (kbd "M-h") 'how-many)
  )

(define-key my-search-prefix (kbd "M-h") 'how-many)


;;; [ swoop ] -- Peculiar buffer navigation for Emacs.

(use-package swoop
  :ensure t
  :config
  (define-key my-search-prefix (kbd "C-o") 'swoop)
  (define-key my-search-prefix (kbd "C-M-o") 'swoop-multi)
  (define-key my-search-prefix (kbd "M-o")   'swoop-pcre-regexp)
  (define-key my-search-prefix (kbd "C-S-o") 'swoop-back-to-last-position)
  )


;;; [ helm-swoop ]

(use-package helm-swoop
  :ensure t
  :config
  ;; Save buffer when helm-multi-swoop-edit complete
  (setq helm-multi-swoop-edit-save t)
  ;; If this value is t, split window inside the current window
  (setq helm-swoop-split-with-multiple-windows t)
  ;; Split direction. 'split-window-vertically or 'split-window-horizontally
  (setq helm-swoop-split-direction 'split-window-vertically)
  ;; If nil, you can slightly boost invoke speed in exchange for text color
  (setq helm-swoop-speed-or-color nil)
  ;; Go to the opposite side of line from the end or beginning of line
  (setq helm-swoop-move-to-line-cycle t)
  ;; Optional face for line numbers
  ;; Face name is `helm-swoop-line-number-face`
  (setq helm-swoop-use-line-number-face nil)

  ;; keybindings
  (define-key my-search-prefix (kbd "C-o") 'helm-swoop)
  (define-key my-search-prefix (kbd "C-S-o") 'helm-swoop-back-to-last-point)
  (define-key my-search-prefix (kbd "C-M-o") 'helm-multi-swoop) ; `helm-multi-swoop-all'
  ;; When doing isearch, hand the word over to helm-swoop
  (define-key isearch-mode-map (kbd "M-o") 'helm-swoop-from-isearch)
  (define-key helm-swoop-map (kbd "M-o") 'helm-multi-swoop-all-from-helm-swoop)
  )


(provide 'init-my-emacs-search-occur)

;;; init-my-emacs-search-occur.el ends here
