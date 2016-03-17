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
  :config
  (global-set-key (kbd "C-c `") 'show-marks)
  )


;;;_ visual-mark


;;; [ ace-jump-mode -- Ace Jump Mode ]

;;; Usage:
;; "C-;" ==> ace-jump-word-mode
;;     enter first character of a word, select the highlighted key to move to it.
;; "C-'" ==> ace-jump-mode-pop-mark
;;     popup the mark to jump back.
;; "C-u C-c SPC" ==> ace-jump-char-mode
;;     enter a character for query, select the highlighted key to move to it.
;; "C-u C-u C-c SPC" ==> ace-jump-line-mode
;;     each non-empty line will be marked, select the highlighted key to move to it.

(use-package ace-jump-mode
  :ensure t
  :config
  ;; enable a more powerful jump back function from ace jump mode
  (autoload 'ace-jump-mode-pop-mark "ace-jump-mode" "Ace jump back:-)" t)
  (eval-after-load "ace-jump-mode" '(ace-jump-mode-enable-mark-sync))

  ;; (global-set-key [remap flyspell-auto-correct-previous-word] nil)
  ;; FIXME: this does not work, conflict with `flyspell-auto-correct-previous-word'.
  (global-set-key (kbd "C-'") 'ace-jump-mode)
  ;; (define-key global-map (kbd "C-'") 'ace-jump-mode-pop-mark)
  )


;;; [ ace-isearch ] -- A seamless bridge between isearch and ace-jump-mode.

;;; ace-isearch.el provides a minor mode which combines isearch and ace-jump-mode.

;;; Usage:
;; The "default" behavior can be summrized as:

;; *Length*
;; - L = 1 : ace-jump-mode
;; - 1 < L < 6 : isearch
;; - L >= 6 : helm-swoop-from-isearch

;; where L is the input string length during isearch. When L is 1, after a few
;; seconds specified by ace-isearch-input-idle-delay, ace-jump-mode will be
;; invoked. Of course you can customize the above behaviour.

(use-package ace-isearch
  :config
  (setq ace-isearch-use-ace-jump t
        ;; ace-isearch-input-idle-delay 0.5
        ;; ace-isearch-input-length 6
        ;; ace-isearch-function-from-isearch 'swoop-from-isearch
        ;; ace-isearch-use-function-from-isearch t
        ;; ace-isearch-set-ace-jump-after-isearch-exit t
        ;; ace-isearch-use-fallback-function 
        )

  ;; (define-key swoop-map (kbd "C-s") 'swoop-action-goto-line-next)
  ;; (define-key swoop-map (kbd "C-r") 'swoop-action-goto-line-prev)

  ;; (ace-isearch-mode +1)
  (global-ace-isearch-mode +1)
  )


;;; [ pophint ]

(use-package pophint
  :config
  ;; (define-key global-map (kbd "C-;") 'pophint:do-flexibly)
  ;; (define-key global-map (kbd "C-+") 'pophint:do)
  ;; (define-key global-map (kbd "M-;") 'pophint:redo)
  ;; (define-key global-map (kbd "C-M-;") 'pophint:do-interactively)
  )


;;; [ scroll ]

(setq scroll-margin 3)


;; [ recenter ]

(setq recenter-positions '(top middle bottom))

(global-set-key (kbd "C-l") 'recenter-top-bottom)


;;; [ Imenu ]

(global-set-key (kbd "C-x j") 'imenu)


;;; [ popup-imenu ]

;; (use-package popup-imenu
;;   :ensure t
;;   :config
;;   (global-set-key [remap imenu] 'popup-imenu)
;;   ;; close the popup with same key.
;;   (define-key popup-isearch-keymap (kbd "C-x j") 'popup-isearch-cancel)
;;   )


(provide 'init-my-emacs-navigation)

;;; init-my-emacs-navigation.el ends here
