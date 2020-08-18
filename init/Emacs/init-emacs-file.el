;;; init-emacs-file.el --- 
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; archive file

(auto-compression-mode 1)           ; auto decompress when open compressed file.

;;; [ vlf ] -- View Large Files

(use-package vlf
  :ensure t
  :defer t
  :commands (vlf vlf-ediff-files vlf-ediff-buffers)
  :config (vlf-setup))

;;; [ so-long ] -- Say farewell to performance problems with minified code.

(use-package so-long
  :ensure t
  ;; Avoid performance issues in files with very long lines.
  :init (global-so-long-mode 1))

;;; [ openwith ] -- Open files with external programs.

;;; FIXME: it cause Emacs auto display Org inline images with external command `display'.
;;
;; (use-package openwith
;;   :ensure t
;;   :defer t
;;   :init (openwith-mode -1) ; disable `openwith' in Org-mode auto open file links.
;;   ;; (setq openwith-confirm-invocation t)
;;   (setq openwith-associations
;;         (append openwith-associations
;;                 (list
;;                  (list (openwith-make-extension-regexp
;;                         '("mpg" "mpeg" "mp3" "mp4"
;;                           "avi" "rmvb" "wmv" "wav" "mov" "flv" "hlv"
;;                           "ogm" "ogg" "ogv" "mkv" "webm"))
;;                        "mplayer" '(file))
;;                  (list (openwith-make-extension-regexp
;;                         '("xbm" "pbm" "pgm" "ppm" "pnm"
;;                           ;; "png" "bmp" "tif" "jpeg" "jpg"
;;                           ))
;;                        "sxiv" '(file))
;;                  (list (openwith-make-extension-regexp
;;                         '("doc" "xls" "ppt" "odt" "ods" "odg" "odp"))
;;                        "libreoffice" '(file))
;;                  '("\\.lyx\\'" "lyx" (file))
;;                  '("\\.chm\\'" "kchmviewer" (file))
;;                  (list (openwith-make-extension-regexp
;;                         '("ps" "ps.gz" "dvi"))
;;                        "zathure" '(file)))))
;;
;;   :config
;;   ;;; NOTE: enable this will cause Org-mode open inline displayed images with external program.
;;   ;; (add-to-list 'openwith-associations '("\\.gif\\'" "gwenview" (file)))
;;   ;; (add-to-list 'openwith-associations '("\\.svg\\'" "feh --magick-timeout 5" (file)))
;;   (add-to-list 'openwith-associations '("\\.swf\\'" "swfdec-player" (file)))
;;   (add-to-list 'openwith-associations '("\\.jar\\'" "java -jar" (file)))
;;
;;   ;; If you also use emacs for email, you may want to add this to your config:
;;   ;; (add-to-list  'mm-inhibit-file-name-handlers 'openwith-file-handler)
;;   )

;;; [ TRAMP ]

(require 'init-tramp)

;;; [ doc-view ] -- View PDF/PostScript/DVI files in Emacs.

(use-package doc-view
  :defer t
  :commands (doc-view-mode))

;;; [ exif ] -- parsing Exif data in JPEG images.

;;; (exif-parse-file "test.jpg")

(use-package exif
  :defer t)

;;; [ exiftool ] -- Elisp wrapper around ExifTool.

(use-package exiftool
  :ensure t
  :defer t
  :commands (exiftool-read exiftool-write exiftool-copy))


(provide 'init-emacs-file)

;;; init-emacs-file.el ends here
