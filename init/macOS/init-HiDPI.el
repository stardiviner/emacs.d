;;; init-HiDPI.el ---  -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-03-03 14:32:44 stardiviner>

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

;;; Override Org customized settings
;;; avoid small Org inline image size
(add-hook 'org-mode-hook (lambda () (setq org-image-actual-width t)))
;; increase AUCTeX: TeX/LaTeX preview scale size.
(add-hook 'latex-mode-hook (lambda () (setq preview-scale-function 4.0)))
;;; avoid small LaTeX preview inline image in Org
(add-hook 'latex-mode-hook (lambda () (setq org-format-latex-options
                                       (plist-put org-format-latex-options :scale 3.0))))



(provide 'init-HiDPI)

;;; init-HiDPI.el ends here
