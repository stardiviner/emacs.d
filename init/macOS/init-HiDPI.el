;;; init-HiDPI.el ---  -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-02-11 19:27:58 stardiviner>

;;; Commentary:



;;; Code:

;;; set font
;; (set-frame-font (format "%s:pixelsize=%d" "DejaVu Sans Mono" 22) t)

(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono"
                    :font "DejaVu Sans Mono 9")

;; (setq org-image-actual-width t)

;; increase TeX/LaTeX preview scale size.
;; (setq preview-scale-function 4)



(provide 'init-HiDPI)

;;; init-HiDPI.el ends here
