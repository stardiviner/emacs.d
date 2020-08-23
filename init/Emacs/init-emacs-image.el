;;; init-emacs-image.el --- init for Image display
;;; -*- coding: utf-8 -*-

;;; Commentary:

;; - variable: `image-file-name-extensions'

;;; Code:

;;; [ image-file ] -- support for visiting image files.

(use-package image-file
  :init (add-to-list 'image-file-name-extensions "webp" 'append)
  :config
  ;; NOTE: this will open image file with "actual size" instead of fit window width.
  ;; auto display image
  ;; (auto-image-file-mode t)
  )

;;; [ image-mode ] -- support for visiting image files.

(use-package image-mode
  :custom (image-use-external-converter t)
  :config
  (define-key image-mode-map (kbd "q") 'kill-current-buffer)

  (defun image-delete-file ()
    "Delete current image file in image-mode."
    (interactive)
    (unless (derived-mode-p 'image-mode)
      (error "The buffer is not in Image mode"))
    (unless buffer-file-name
      (error "The current image is not associated with a file"))
    (delete-file (buffer-file-name))
    (image-next-file))

  (define-key image-mode-map (kbd "D") 'image-delete-file))

;;; [ iimage ] -- inline image minor mode.

;; (use-package iimage
;;   :ensure t
;;   :defer t
;;   :init
;;   ;; enable iimage-mode in some modes
;;   (add-hook 'info-mode-hook #'iimage-mode)
;;   (add-hook 'wiki-mode-hook #'iimage-mode)
;;   (add-hook 'eshell-mode-hook 'iimage-mode))

;;; [ blimp ] -- Bustling Image Manipulation Package; a complete wrapper around all imagemagick commands.

(use-package blimp
  :ensure t
  :commands (blimp-interface)
  :bind (:map image-mode-map ("C-c C-i" . blimp-interface))
  :config (add-hook 'image-mode-hook 'blimp-mode))


(provide 'init-emacs-image)

;;; init-emacs-image.el ends here
