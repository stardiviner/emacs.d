;;; init-emacs-frame.el --- init Emacs frame
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; default frame size

(setq default-frame-alist
      `((top . 0) (left . 0)
        (height . ,(display-pixel-height))
        (width . ,(display-pixel-width))
        (left-fringe) (right-fringe)
        (tool-bar-lines . 0)
        (menu-bar-lines . 0)
        (scroll-bar . nil)
        (vertical-scroll-bars . nil)))

(setq initial-frame-alist default-frame-alist)
(setq window-system-default-frame-alist default-frame-alist)
(setq minibuffer-frame-alist default-frame-alist)

;; Every time a window is started, make sure it get maximized
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))


;; (setq frame-resize-pixelwise nil)

(global-set-key [remap toggle-frame-maximized] 'toggle-frame-fullscreen)

;;; scroll other frame like `scroll-other-window' command.
(defun my/scroll-other-frame ()
  "Scroll other frame.
This is helpful for multiple monitor screens."
  (interactive)
  (other-frame +1)
  (if (derived-mode-p 'eaf-mode)
      (eaf-proxy-scroll_up_page)
    (call-interactively 'scroll-up-command))
  (other-frame -1))

(defun my/scroll-other-frame-down ()
  "Scroll other frame down.
This is helpful for multiple monitor screens."
  (interactive)
  (other-frame +1)
  (if (derived-mode-p 'eaf-mode)
      (eaf-proxy-scroll_down_page)
    (scroll-down-command))
  (other-frame -1))

(global-set-key (kbd "C-M-]") 'my/scroll-other-frame)
(global-set-key (kbd "C-M-[") 'my/scroll-other-frame-down)


(provide 'init-emacs-frame)

;;; init-emacs-frame.el ends here
