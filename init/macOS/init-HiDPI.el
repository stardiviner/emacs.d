;;; init-HiDPI.el ---  -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-02-10 21:59:39 stardiviner>

;;; Commentary:



;;; Code:

;;; set font
(set-frame-font (format "%s:pixelsize=%d" "DejaVu Sans Mono" 22) t)
(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono"
                    :font "DejaVu Sans Mono 18")

(set-face-attribute 'variable-pitch nil
                    :family "DejaVu Sans Mono"
                    :font "DejaVu Sans Mono 18"
                    :weight 'normal :slant 'normal)
(set-face-attribute 'fixed-pitch nil
                    :family "DejaVu Sans Mono"
                    :font "DejaVu Sans Mono 18"
                    :weight 'normal :slant 'normal)
(set-face-attribute 'fixed-pitch-serif nil
                    :family "DejaVu Serif"
                    :font "DejaVu Sans Mono 18"
                    :weight 'normal :slant 'italic)

(set-face-attribute 'org-level-1 nil
                    :font "DejaVu Sans Mono 22")
(set-face-attribute 'org-level-2 nil
                    :font "DejaVu Sans Mono 20")



(provide 'init-HiDPI)

;;; init-HiDPI.el ends here
