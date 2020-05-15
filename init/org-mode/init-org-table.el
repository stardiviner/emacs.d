;;; init-org-table.el --- init for Org Table
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(add-to-list 'display-buffer-alist
             '("\^\\*Org Table Edit Field\\*" (display-buffer-reuse-window display-buffer-below-selected)))

;;; display table header when invisible.
;;; Activate `org-table-electric-header-mode' by default?
(setq org-table-header-line-p nil)

;;; correctly align Chinese/English mixed table.
;; set CJK font
(when (display-graphic-p)
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (cl-case system-type
                        ('gnu/linux
                         (font-spec :family "Sarasa Gothic SC" :size 28)
                         ;; (font-spec :family "Source Han Sans SC" :size 28)
                         ;; (font-spec :family "Noto Sans CJK SC" :size 16)
                         ;; (font-spec :family "Hack" :size 12)
                         ;; (font-spec :family "WenQuanYi Micro Hei" :size 12)
                         ;; (font-spec :family "NSimSun" :size 14)
                         ;; (font-spec :family "BabelStone Han" :size 14)
                         ;; (font-spec :family "FZQingKeBenYueSongS-R-GB" :size 15) ; 方正清刻本悦宋简体
                         ;; (font-spec :family "WenYue GuDianMingChaoTi-R-GB" :size 15)
                         ;; (font-spec :family "WenYue HouXianDaiTi-R-GB" :size 15)
                         ;; (font-spec :family "Roboto" :size 15)
                         ;; (font-spec :family "WenyueType GutiFangsong-R-GB" :size 15)
                         )
                        ('darwin
                         ;; macOS Chinese fonts
                         ;; (font-spec :family "Songti SC" :size 15)
                         ;; (font-spec :family "Heiti SC" :size 15)
                         ;; (font-spec :family "Hiragino Sans CNS" :size 15)
                         ;; (font-spec :family "Lantinghei SC" :size 15)
                         ;; (font-spec :family "LiHei Pro" :size 15)
                         ;; (font-spec :family "LiSong Pro" :size 15)
                         ;; (font-spec :family "PingFang SC" :size 15)
                         ;; (font-spec :family "Yuanti SC" :size 15)
                         ;; (font-spec :family "Apple LiGothic" :size 15)

                         ;; macOS handwriting fonts
                         ;; (font-spec :family "BiauKai" :size 13)
                         (font-spec :family "Kaiti SC" :size 14)
                         ;; (font-spec :family "Hannotate SC" :size 14)
                         ;; (font-spec :family "Libian SC" :size 14)
                         ;; (font-spec :family "Weibei SC" :size 14)
                         ;; (font-spec :family "LingWai SC" :size 15)
                         ;; (font-spec :family "HanziPen SC" :size 14)
                         ;; (font-spec :family "Yuppy SC" :size 14)
                         ;; (font-spec :family "Wawati SC" :size 14)
                         ;; (font-spec :family "Xingkai SC" :size 14)
                         )))))

;;; Text Scale
;; (cl-case system-type
;;   ('gnu/linux
;;    (setq face-font-rescale-alist '(("WenQuanYi Micro Hei" . 1.3))))
;;   ('darwin
;;    (setq face-font-rescale-alist '(("Kaiti SC" . 1.0)))))

;; [ org-plot ] -- Plotting Tables in Org-mode.

(use-package org-plot
  :defer t
  :commands (org-plot/gnuplot))

;;; Org Table translator functions.
(add-to-list 'org-default-properties "ORGTBL") ; for Org-mode Table translator functions.

;; define a keybinding for org table translator functions
(define-key org-mode-map (kbd "C-c \" i") 'orgtbl-insert-radio-table)
(define-key org-mode-map (kbd "C-c \" s") 'orgtbl-send-table)

;;; [ orgtbl-ascii-plot ] -- ascii-art bar plots in org-mode tables.

;; #+TBLFM: $4='(orgtbl-ascii-draw plot min max)
;;
;; - #+TBLFM: $4='(orgtbl-ascii-draw $2 0 10)

(use-package orgtbl-ascii-plot
  :ensure t
  :defer t
  :commands (orgtbl-ascii-plot))

;;; [ orgtbl-join ] -- join two Org-mode tables.

(use-package orgtbl-join
  :ensure t
  :defer t
  :commands (orgtbl-join orgtbl-to-joined-table org-insert-dblock:join))

;;; import .xlsx, .csv file into Org.
(defun org-table-import-xlsx-to-csv-org ()
  (interactive)
  (let* ((source-file  (file-name-sans-extension (buffer-file-name (current-buffer))))
         (xlsx-file (concat source-file ".xlsx"))
         (csv-file (concat source-file ".csv")))
    (org-odt-convert xlsx-file "csv")
    (org-table-import csv-file  nil)))

(defun org-table-import-xlsx-file-to-csv-org (file)
  "Import .xlsx, .csv `FILE' into Org."
  (interactive "f")
  (let* ((source-file  (file-name-sans-extension (buffer-file-name (current-buffer))))
         (xlsx-file (concat source-file ".xlsx"))
         (csv-file (concat source-file ".csv")))
    (org-odt-convert file "csv")
    (org-table-import csv-file  nil)))

;;; [ org-transform-tree-table ] -- Transform an org-mode outline and its properties to a table format (org-table, CSV).

(use-package org-transform-tree-table
  :ensure t
  :defer t
  :commands (org-transform-tree-table/toggle))

;;; [ mysql-to-org-mode ] -- Minor mode for emacs to output the results of mysql queries to org tables.

(use-package mysql-to-org
  :ensure t
  :defer t
  :commands (mysql-to-org-eval mysql-to-org-eval-string-at-point mysql-to-org-mode))



(provide 'init-org-table)

;;; init-org-table.el ends here
