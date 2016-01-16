;;; init-my-emacs-image.el --- init for Image display
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ auto-image-file-mode ]

;; (setq image-file-name-extensions '("png" "jpeg" "jpg" "gif" "tiff" "tif" "xbm" "xpm" "pbm" "pgm" "ppm" "pnm" "svg"))

(auto-image-file-mode t) ;; auto display image


;;; [ iimage-mode ]

(if (not (featurep 'iimage))
    (require 'iimage))

;; enable `iimage-mode' globally.
;; (iimage-mode 1)

;;; for eshell,
;; auto display image in eshell for links after command `cat'.
;; https://emacs.stackexchange.com/questions/3432/display-images-in-eshell-with-iimage-mode

;; enable iimage-mode in eshell.
(add-hook 'eshell-mode-hook 'iimage-mode)

(defun my/iimage-mode-refresh--eshell/cat (orig-fun &rest args)
  "Display image when using cat on it."
  (let ((image-path (cons default-directory iimage-mode-image-search-path)))
    (dolist (arg args)
      (let ((imagep nil)
            file)
        (with-silent-modifications
          (save-excursion
            (dolist (pair iimage-mode-image-regex-alist)
              (when (and (not imagep)
                       (string-match (car pair) arg)
                       (setq file (match-string (cdr pair) arg))
                       (setq file (locate-file file image-path)))
                (setq imagep t)
                (add-text-properties 0 (length arg)
                                     `(display ,(create-image file)
                                               modification-hooks
                                               (iimage-modification-hook))
                                     arg)
                (eshell-buffered-print arg)
                (eshell-flush)))))
        (when (not imagep)
          (apply orig-fun (list arg)))))
    (eshell-flush)))

(advice-add 'eshell/cat :around #'my/iimage-mode-refresh--eshell/cat)

;; enable iimage-mode in some modes
(add-hook 'info-mode-hook #'iimage-mode)
(add-hook 'wiki-mode-hook #'iimage-mode)

(defun refresh-iimages ()
  "Only way I've found to refresh iimages (without also recentering)"
  (interactive)
  (clear-image-cache nil)
  (iimage-mode nil)
  (iimage-mode t)
  (message "Refreshed images."))

;; update images (which is not done on the fly) in a hook after compilation.
;; e.g. you can see the image in the compilation buffer like Python code compilation.
(add-to-list 'compilation-finish-functions 
             (lambda (buffer msg)
               (save-excursion
                 (set-buffer buffer)
                 (refresh-iimages))))


;;; [ image+ ] -- Emacs image extension

;; - [C-x C-l] :: prefix
;; - `imagex-sticky-mode'
;; - `imagex-global-sticky-mode'
;;   (eval-after-load 'image+ '(imagex-global-sticky-mode 1))
;; - [M-x imagex-auto-adjust-mode]
;; - (setq imagex-quiet-error t)

;; (eval-after-load 'image
;;   '(require 'image+))
;;
;; (eval-after-load 'image+
;;   `(when (require 'hydra nil t)
;;      (defhydra imagex-sticky-binding (global-map "C-x C-l")
;;        "Manipulating Image"
;;        ("+" imagex-sticky-zoom-in "zoom in")
;;        ("-" imagex-sticky-zoom-out "zoom out")
;;        ("M" imagex-sticky-maximize "maximize")
;;        ("O" imagex-sticky-restore-original "restore original")
;;        ("S" imagex-sticky-save-image "save file")
;;        ("r" imagex-sticky-rotate-right "rotate right")
;;        ("l" imagex-sticky-rotate-left "rotate left"))))




(provide 'init-my-emacs-image)

;;; init-my-emacs-image.el ends here
