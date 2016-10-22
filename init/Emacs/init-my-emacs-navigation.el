;;; init-my-emacs-navigation.el --- init Emacs Navigation.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ movement ]

(setq sentence-end-double-space nil)

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

;; When popping the mark, continue popping until the cursor
;; actually moves
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))


;;;_ show-marks

(use-package show-marks
  :ensure t
  :defer t
  :init
  (global-set-key (kbd "C-c `") 'show-marks)
  )


;;; [ ace-jump-mode -- Ace Jump Mode ]

(use-package ace-jump-mode
  :ensure t
  :defer t
  :init
  ;; enable a more powerful jump back function from ace jump mode
  (autoload 'ace-jump-mode-pop-mark "ace-jump-mode" "Ace jump back:-)" t)
  (eval-after-load "ace-jump-mode" '(ace-jump-mode-enable-mark-sync))

  (global-set-key (kbd "C-'") 'ace-jump-mode)
  ;; (define-key global-map (kbd "C-'") 'ace-jump-mode-pop-mark)
  )


;;; [ scroll ]

(setq scroll-margin 3)

(setq fast-but-imprecise-scrolling t)


;; [ recenter ]

(setq recenter-positions '(top middle bottom))

(global-set-key (kbd "C-l") 'recenter-top-bottom)


;;; [ Imenu ]

(use-package imenu
  :ensure t
  :defer t
  :init
  (global-set-key (kbd "C-x j") 'imenu)
  :config
  (setq imenu-auto-rescan t)
  )


(provide 'init-my-emacs-navigation)

;;; init-my-emacs-navigation.el ends here
