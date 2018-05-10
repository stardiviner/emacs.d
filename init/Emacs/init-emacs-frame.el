;;; init-emacs-frame.el --- init Emacs frame
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; default frame size

(setq default-frame-alist
      '((top . 0) (left . 0)
        (height . 120)
        (width . 100)
        (left-fringe) (right-fringe)
        (tool-bar-lines . 0)
        (menu-bar-lines . 0)
        (scroll-bar . nil)
        (vertical-scroll-bars . nil)
        ))

(setq initial-frame-alist default-frame-alist)
(setq window-system-default-frame-alist default-frame-alist)
(setq minibuffer-frame-alist default-frame-alist)

;; Every time a window is started, make sure it get maximized
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))


;; (setq frame-resize-pixelwise nil)

(global-set-key [remap toggle-frame-maximized] 'toggle-frame-fullscreen)

(defun my:turn-current-window-into-new-frame ()
  "Popup current window to another new frame."
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))
(global-set-key (kbd "C-x 5 5") 'my:turn-current-window-into-new-frame)


(provide 'init-emacs-frame)

;;; init-emacs-frame.el ends here
