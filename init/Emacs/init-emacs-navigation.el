;;; init-emacs-navigation.el --- init Emacs Navigation.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ scroll ]

(setq scroll-margin 3
      scroll-preserve-screen-position 3
      scroll-conservatively 100000
      fast-but-imprecise-scrolling t)

;;; [ pixel-scroll-mode ]

;;; Scrolling.
;; Good speed and allow scrolling through large images (pixel-scroll).
;; Note: Scroll lags when point must be moved but increasing the number
;;       of lines that point moves in pixel-scroll.el ruins large image
;;       scrolling. So unfortunately I think we'll just have to live with
;;       this.
(pixel-scroll-mode)
(setq pixel-dead-time 0) ; Never go back to the old scrolling behaviour.
(setq pixel-resolution-fine-flag t) ; Scroll by number of pixels instead of lines (t = frame-char-height pixels).
(setq mouse-wheel-scroll-amount '(1)) ; Distance in pixel-resolution to scroll each mouse wheel event.
(setq mouse-wheel-progressive-speed nil) ; Progressive speed is too fast for me.

;; [ recenter ]

(setq recenter-positions '(top middle bottom))

;;; [ centered-cursor-mode ] -- cursor stays vertically centered.

(use-package centered-cursor-mode
  :ensure t
  :defer t
  :commands (global-centered-cursor-mode centered-cursor-mode)
  :hook (after-init . global-centered-cursor-mode))

;;; [ movement ]

(setq track-eol t) ; always track end of line when moving at end of line.

;; set sentence-end to recognize chinese punctuation.
;; (setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")


;;; [ Mark ] --- [C-SPC / C-@] + [C-u C-SPC / C-u C-@] + [C-`] / [M-`]

(setq set-mark-command-repeat-pop t)

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region.
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(global-set-key (kbd "C-`") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(global-set-key (kbd "M-`") 'jump-to-mark)

;; (defun exchange-point-and-mark-no-activate ()
;;   "Identical to \\[exchange-point-and-mark] but will not activate the region."
;;   (interactive)
;;   (exchange-point-and-mark)
;;   (deactivate-mark nil))
;; (define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

(defadvice pop-to-mark-command (around ensure-new-position activate)
  "When popping the mark, continue popping until the cursor actually moves."
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))


;;; [ ace-jump-mode -- Ace Jump Mode ]

(use-package ace-jump-mode
  :ensure t
  :defer t
  :commands (ace-jump-mode)
  :bind (("C-'" . ace-jump-mode) :map org-mode-map ("C-'" . ace-jump-mode))
  :init (setq ace-jump-mode-scope 'window)
  ;; enable a more powerful jump back function from ace jump mode
  (setq ace-jump-sync-emacs-mark-ring t))


(provide 'init-emacs-navigation)

;;; init-emacs-navigation.el ends here
