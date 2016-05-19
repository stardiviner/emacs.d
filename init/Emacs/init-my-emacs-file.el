;;; init-my-emacs-file.el --- 
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; archive file

(auto-compression-mode 1)           ; auto decompress when open compressed file.


;;; [ DovView mode ]

;; Emacs utility for viewing PDF/PS/DVI/OpenDocument/Microsoft Office documents files in Emacs.
;; Usage:
;; When you visit a document file that can be displayed with DocView mode, Emacs auto use DocView mode.
;; - [M-x doc-view]
;; - `doc-view-clear-cache' :: to delete all cached files.
;; - `doc-view-dired' :: for Dired users.

(require 'doc-view)

(setq doc-view-conversion-refresh-interval 1)


;;; large file

;;; [ vlf -- View Large Files ]

;;; Usage:
;;; - [M-x vlf PATH-TO-FILE]
;;; - [C-c C-v] -- prefix.

;; (use-package vlf
;;   :ensure t
;;   :config
;;   ;; All VLF operations are grouped under the C-c C-v prefix by default. Here’s
;;   ;; example how to add another prefix (C-x v):
;;   ;; (eval-after-load "vlf"
;;   ;;   '(define-key vlf-prefix "\C-xv" vlf-mode-map))
;;   )


;;; [ openwith ]

(use-package openwith
  :ensure t
  :config
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4"
                  "avi" "wmv" "wav" "mov" "flv"
                  "ogm" "ogg" "mkv"))
               "mplayer"
               '(file))
         (list (openwith-make-extension-regexp
                '("xbm" "pbm" "pgm" "ppm" "pnm"
                  ;; don't open normal images with external program. use Emacs
                  ;; buffer to display inline image.
                  ;;
                  ;; "png" "gif" "bmp" "tif" "jpeg" "jpg"
                  ))
               "sxiv"
               '(file))
         (list (openwith-make-extension-regexp
                '("doc" "xls" "ppt" "odt" "ods" "odg" "odp"))
               "libreoffice"
               '(file))
         '("\\.lyx\\'" "lyx" (file))
         '("\\.chm\\'" "kchmviewer" (file))
         (list (openwith-make-extension-regexp
                '("ps" "ps.gz" "dvi"
                  ;; "pdf"
                  ))
               "okular"
               '(file))
         ))

  (add-to-list 'openwith-associations '("\\.gif\\'" "gwenview" (file)))
  (add-to-list 'openwith-associations '("\\.svg\\'" "feh --magick-timeout 5" (file)))
  (add-to-list 'openwith-associations '("\\.swf\\'" "swfdec-gnome" (file)))
  (add-to-list 'openwith-associations '("\\.jar\\'" "java -jar" (file)))
  
  ;; If you also use emacs for email, you may want to add this to your config:
  ;; (add-to-list  'mm-inhibit-file-name-handlers 'openwith-file-handler)
  
  (openwith-mode 1)
  )


(provide 'init-my-emacs-file)

;;; init-my-emacs-file.el ends here
