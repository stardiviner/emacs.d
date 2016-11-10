;;; init-my-emacs-file.el --- 
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; archive file

(auto-compression-mode 1)           ; auto decompress when open compressed file.


;;; [ vlf -- View Large Files ]

(use-package vlf
  :ensure t
  :config
  ;; All VLF operations are grouped under the C-c C-v prefix by default. Here’s
  ;; example how to add another prefix (C-x v):
  ;; (with-eval-after-load "vlf"
  ;;   (define-key vlf-prefix "\C-xv" vlf-mode-map))
  )


;;; [ openwith ]

(use-package openwith
  :ensure t
  :defer t
  :init
  (openwith-mode 1)
  
  :config
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4"
                  "avi" "wmv" "wav" "mov" "flv"
                  "ogm" "ogg" "mkv"))
               "mplayer" '(file))
         (list (openwith-make-extension-regexp
                '("xbm" "pbm" "pgm" "ppm" "pnm"))
               ;; don't open normal images with external program. use Emacs
               ;; buffer to display inline image.
               ;;
               ;; "png" "gif" "bmp" "tif" "jpeg" "jpg"
               "sxiv" '(file))
         (list (openwith-make-extension-regexp
                '("doc" "xls" "ppt" "odt" "ods" "odg" "odp"))
               "libreoffice" '(file))
         '("\\.lyx\\'" "lyx" (file))
         '("\\.chm\\'" "kchmviewer" (file))
         (list (openwith-make-extension-regexp
                '("ps" "ps.gz" "dvi"))
               ;; "pdf"
               "okular" '(file))))
  

  ;;; NOTE: enable this will cause Org-mode open inline displayed images with external program.
  ;; (add-to-list 'openwith-associations '("\\.gif\\'" "gwenview" (file)))
  ;; (add-to-list 'openwith-associations '("\\.svg\\'" "feh --magick-timeout 5" (file)))
  (add-to-list 'openwith-associations '("\\.swf\\'" "swfdec-player" (file)))
  (add-to-list 'openwith-associations '("\\.jar\\'" "java -jar" (file)))
  ;; If you also use emacs for email, you may want to add this to your config:
  ;; (add-to-list  'mm-inhibit-file-name-handlers 'openwith-file-handler)
  )

;;; [ snapshot-timemachine ] -- mode to step through (Btrfs, ZFS, ...) snapshots of files.

(use-package snapshot-timemachine
  :ensure t)


(provide 'init-my-emacs-file)

;;; init-my-emacs-file.el ends here
