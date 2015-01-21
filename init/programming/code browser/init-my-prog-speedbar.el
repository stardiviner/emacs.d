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

(setq sr-speedbar-width 30
      sr-speedbar-width-x 30
      sr-speedbar-width-console 30
      sr-speedbar-max-width 30
      sr-speedbar-delete-windows nil ; delete other windows before showing sr-speedbar up
      sr-speedbar-skip-other-window-p nil ; skip sr-speedbar in other-window cycle.
      sr-speedbar-auto-refresh t
      sr-speedbar-right-side nil ; nil for left. t for righ.t
      )

(add-hook 'speedbar-mode-hook
          (lambda ()
            (linum-mode -1)))




(defun my-sr-speedbar-toggle-and-switch ()
  "Toggle sr-speedbar or switch to sr-speedbar window if already opened."
  (interactive)
  ;; (if (get-buffer "*SPEEDBAR*")
  ;;     (switch-to-buffer "*SPEEDBAR*")
  ;;   (sr-speedbar-toggle)
  ;;   (bury-buffer)
  ;;   (switch-to-buffer "*SPEEDBAR*"))
  (if (sr-speedbar-exist-p)
      ;; (sr-speedbar-close)
      (sr-speedbar-select-window)
    (sr-speedbar-open)
    (other-window 1) ; switch back to previous window.
    )
  )

;; (global-set-key [f8] 'my-sr-speedbar-toggle-and-switch)
(global-set-key [f8] 'sr-speedbar-toggle)



(provide 'init-my-prog-speedbar)

;;; init-my-prog-speedbar.el ends here
