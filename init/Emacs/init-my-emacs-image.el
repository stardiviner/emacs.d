;;; init-my-emacs-image.el --- init for Image display
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ auto-image-file-mode ]

;; (setq image-file-name-extensions
;;       '("png" "jpeg" "jpg" "gif"
;;         "tiff" "tif" "xbm" "xpm" "pbm" "pgm" "ppm" "pnm"
;;         "svg"))

;; auto display image
(auto-image-file-mode t)


;;; [ iimage-mode ]

(use-package iimage
  :ensure t
  :defer t
  :init
  ;; enable iimage-mode in some modes
  (add-hook 'info-mode-hook #'iimage-mode)
  (add-hook 'wiki-mode-hook #'iimage-mode)
  (add-hook 'eshell-mode-hook 'iimage-mode)
  )


;;; [ image+ ] -- Emacs image extension

;; (use-package image+
;;   :ensure t
;;   :defer t
;;   :config
;;   (with-eval-after-load 'image+
;;     (when (require 'hydra nil t)
;;       (defhydra imagex-sticky-binding (global-map "C-x C-l")
;;         "Manipulating Image"
;;         ("+" imagex-sticky-zoom-in "zoom in")
;;         ("-" imagex-sticky-zoom-out "zoom out")
;;         ("M" imagex-sticky-maximize "maximize")
;;         ("O" imagex-sticky-restore-original "restore original")
;;         ("S" imagex-sticky-save-image "save file")
;;         ("r" imagex-sticky-rotate-right "rotate right")
;;         ("l" imagex-sticky-rotate-left "rotate left"))))
;;   )

;;; [ picpocket ] -- image viewer

(use-package picpocket
  :ensure t)


(provide 'init-my-emacs-image)

;;; init-my-emacs-image.el ends here
