;;; init-my-tool-screencast.el --- init for Screencast usage
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'screencast-prefix)
  (define-prefix-command 'screencast-prefix))
(define-key my-tools-prefix (kbd "M-s") 'screencast-prefix)

;;; [ command-log-mode ] -- log commands to buffer

(use-package command-log-mode
  :ensure t
  :defer t
  :bind (:map screencast-prefix
              ("M-k" . clm/toggle-command-log-buffer)
              ("k" . command-log-mode)
              ("K" . global-command-log-mode))
  :init
  ;; FIXME: disable default global keybinding [C-c o]
  (setq command-log-mode-key-binding-open-log nil)
  :config
  (setq command-log-mode-auto-show t
        command-log-mode-is-global nil
        command-log-mode-open-log-turns-on-mode t
        ;; command-log-mode-window-size 40
        command-log-mode-window-font-size 2
        )
  )


;;; [ camcorder ] -- Tool for capturing screencasts directly from Emacs (use FFmpeg as backend)

(use-package camcorder
  :ensure t
  :defer t
  :bind (:map screencast-prefix
              ("r" . camcorder-mode)
              ("R" . camcorder-record))
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
