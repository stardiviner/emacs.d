;;; init-HiDPI.el ---  -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-03-20 23:30:38 stardiviner>

;;; Commentary:



;;; Code:

;;; [ variable-pitch-mode ]

(set-face-attribute 'variable-pitch nil
                    :family "DejaVu Sans Mono"
                    :height 100
                    :weight 'normal :slant 'normal)
(set-face-attribute 'fixed-pitch nil
                    :family "DejaVu Sans Mono"
                    :height 100
                    :weight 'normal :slant 'normal)
(set-face-attribute 'fixed-pitch-serif nil
                    :family "DejaVu Serif"
                    :height 100
                    :weight 'normal :slant 'italic)

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
