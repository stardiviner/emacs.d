;;; init-my-prog-speedbar.el --- init speedbar
;;; -*- coding: utf-8 -*-

;;; Commentary:


;;; Code:

;;; [ speedbar ]

(require 'speedbar)

;; (speedbar 1)


;;; [ Sr-Speedbar ] -- SrSpeedbar is mode make SpeedBar show in Current Frame

;;; Usage:
;; - [sr-speedbar-toggle] :: toggle sr-speedbar window.
;; - [sr-speedbar-select-window] :: select sr-speedbar window.

(require 'sr-speedbar)

(setq sr-speedbar-width 27
      sr-speedbar-width-x 27
      sr-speedbar-width-console 27
      sr-speedbar-max-width 30
      sr-speedbar-delete-windows nil ; delete other windows before showing sr-speedbar up
      sr-speedbar-skip-other-window-p nil ; skep sr-speedbar in other-window cycle.
      sr-speedbar-auto-refresh t
      sr-speedbar-right-side nil ; nil for left. t for righ.t
      )

(add-hook 'speedbar-mode-hook
          (lambda ()
            (linum-mode -1)))



;;; startup in Emacs
;; (sr-speedbar-open)
;; ;; called after `sr-speedbar-open'
;; (with-current-buffer sr-speedbar-buffer-name
;;   (setq window-size-fixed 'width))


(global-set-key [f9] 'sr-speedbar-toggle)



(provide 'init-my-prog-speedbar)

;;; init-my-prog-speedbar.el ends here
