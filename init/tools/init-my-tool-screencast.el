;;; init-my-tool-screencast.el --- init for Screencast usage
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ command-log-mode ] -- log commands to buffer

(use-package command-log-mode
  :ensure t
  :defer t
  :init
  ;; FIX: disable default global keybinding [C-c o]
  (setq command-log-mode-key-binding-open-log nil)
  
  (define-key my-screenshot-map (kbd "M-k") 'clm/toggle-command-log-buffer)
  (define-key my-screenshot-map (kbd "k") 'command-log-mode)
  (define-key my-screenshot-map (kbd "K") 'global-command-log-mode)
  :config
  (setq command-log-mode-auto-show t
        command-log-mode-is-global nil
        command-log-mode-open-log-turns-on-mode t
        ;; command-log-mode-window-size 40
        command-log-mode-window-font-size 2
        )

  ;; (set-face-attribute 'command-log-key nil
  ;;                     :background "gray" :foreground "red"
  ;;                     )
  ;; (set-face-attribute 'command-log-command nil
  ;;                     :background nil :foreground "black"
  ;;                     )
  )


;;; [ camcorder ] -- Tool for capturing screencasts directly from Emacs (use FFmpeg as backend)

(use-package camcorder
  :ensure t
  :defer t
  :init
  (define-key my-screenshot-map (kbd "r") 'camcorder-mode)
  (define-key my-screenshot-map (kbd "R") 'camcorder-record)
  
  :config
  ;; (setq camcorder-recording-command
  ;;       '("recordmydesktop" " --fps 20 --no-sound --windowid " window-id " -o " file))
  
  (setq camcorder-frame-parameters ; see `make-frame', `default-frame-alist'
        '((name . "camcorder.el Recording - F12 to Stop - F11 to Pause/Resume")
          (height . 150)
          (width . 90)
          ;; (top .  80)
          (minibuffer . t)
          )
        ;; window-id-offset 0
        )
  )


(provide 'init-my-tool-screencast)

;;; init-my-tool-screencast.el ends here
