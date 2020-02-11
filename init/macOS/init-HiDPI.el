;;; init-HiDPI.el ---  -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-02-12 02:31:09 stardiviner>

;;; Commentary:



;;; Code:

;;; set font
;; (set-frame-font (format "%s:pixelsize=%d" "DejaVu Sans Mono" 22) t)

(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono"
                    :font "DejaVu Sans Mono 9")

(when (display-graphic-p)
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (font-spec :family "Noto Sans CJK SC" :size 28))))

;; (setq org-image-actual-width t)

;; increase TeX/LaTeX preview scale size.
;; (setq preview-scale-function 4)



(provide 'init-HiDPI)

;;; init-HiDPI.el ends here
