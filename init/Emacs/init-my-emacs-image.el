;;; init-my-emacs-image.el --- init for Image display
;;; -*- coding: utf-8 -*-

;;; Commentary:

;; - variable: `image-file-name-extensions'

;;; Code:

;;; [ auto-image-file-mode ]

;; (setq image-file-name-extensions
;;       '("png" "jpeg" "jpg" "gif"
;;         "tiff" "tif" "xbm" "xpm" "pbm" "pgm" "ppm" "pnm"
;;         "svg"))

;; auto display image
;; (auto-image-file-mode t) ; NOTE: this will open image file with "actual size" instead of fit window width.


;;; [ iimage ] -- inline image minor mode.

(use-package iimage
  :ensure t
  :defer t
  :init
  ;; enable iimage-mode in some modes
  (add-hook 'info-mode-hook #'iimage-mode)
  (add-hook 'wiki-mode-hook #'iimage-mode)
  (add-hook 'eshell-mode-hook 'iimage-mode)
  )

;;; [ picpocket ] -- image viewer

(use-package picpocket
  :ensure t
  :commands (picpocket)
  )


(provide 'init-my-emacs-image)

;;; init-my-emacs-image.el ends here
