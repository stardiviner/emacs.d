;;; init-my-org-table.el --- init for Org Table
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(setq org-enable-table-editor t)


;;; font size config for Org-mode table chinese & english mixture.

;; (when (window-system)
;;   (defvar emacs-english-font "DejaVu Sans Mono"
;;     "The font name of English.")
;;
;;   (defvar emacs-cjk-font "文泉驛等寬微米黑" "The font name for CJK.")
;;   (setq emacs-cjk-font "WenQuanYi Micro Hei Mono")
;;
;;   (defvar emacs-font-size-pair '(12 . 14)
;;     "Default font size pair for (english . chinese)")
;;
;;   (defvar emacs-font-size-pair-list
;;     '(( 5 .  6) (9 . 10) (10 . 12)(12 . 14)
;;       (13 . 16) (15 . 18) (17 . 20) (19 . 22)
;;       (20 . 24) (21 . 26) (24 . 28) (26 . 32)
;;       (28 . 34) (30 . 36) (34 . 40) (36 . 44))
;;     "This list is used to store matching (englis . chinese) font-size.")
;;
;;   (defun font-exist-p (fontname)
;;     "Test if this font is exist or not."
;;     (if (or (not fontname) (string= fontname ""))
;;         nil
;;       (if (not (x-list-fonts fontname)) nil t)))
;;
;;   (defun set-font (english chinese size-pair)
;;     "Setup emacs English and Chinese font on x window-system."
;;
;;     (if (font-exist-p english)
;;         (set-frame-font (format "%s:pixelsize=%d" english (car size-pair)) t))
;;
;;     (if (font-exist-p chinese)
;;         (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;           (set-fontset-font (frame-parameter nil 'font) charset
;;                             (font-spec :family chinese :size (cdr size-pair))))))
;;   ;; Setup font size based on emacs-font-size-pair
;;   (set-font emacs-english-font emacs-cjk-font emacs-font-size-pair)
;;
;;   (defun emacs-step-font-size (step)
;;     "Increase/Decrease emacs's font size."
;;     (let ((scale-steps emacs-font-size-pair-list))
;;       (if (< step 0) (setq scale-steps (reverse scale-steps)))
;;       (setq emacs-font-size-pair
;;             (or (cadr (member emacs-font-size-pair scale-steps))
;;                 emacs-font-size-pair))
;;       (when emacs-font-size-pair
;;         (message "emacs font size set to %.1f" (car emacs-font-size-pair))
;;         (set-font emacs-english-font emacs-cjk-font emacs-font-size-pair))))
;;
;;   (defun increase-emacs-font-size ()
;;     "Decrease emacs's font-size acording emacs-font-size-pair-list."
;;     (interactive) (emacs-step-font-size 1))
;;
;;   (defun decrease-emacs-font-size ()
;;     "Increase emacs's font-size acording emacs-font-size-pair-list."
;;     (interactive) (emacs-step-font-size -1))
;;
;;   (global-set-key (kbd "C-+") 'increase-emacs-font-size)
;;   (global-set-key (kbd "C--") 'decrease-emacs-font-size)
;;   )


;;; [ orgtbl-ascii-plot ] -- ascii-art bar plots in org-mode tables.

;; #+TBLFM: $4='(orgtbl-ascii-draw plot min max)
;;
;; - #+TBLFM: $4='(orgtbl-ascii-draw $2 0 10)

(use-package orgtbl-ascii-plot
  :ensure t
  :defer t)

;;; [ org-table-sticky-header ] -- Sticky header for org-mode tables.

(use-package org-table-sticky-header
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-table-sticky-header-mode)
  )


(provide 'init-my-org-table)

;;; init-my-org-table.el ends here
