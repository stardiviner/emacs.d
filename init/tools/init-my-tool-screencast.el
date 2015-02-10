;;; init-my-tool-screencast.el --- init for Screencast usage
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ command-log-mode ] -- log commands to buffer

;;; This package is a fork of mwe-log-commands.

;;; Usage:
;;
;; 1. enable command-log-mode at first.
;;    [M-x command-log-mode] :: open and close buffer.
;; - `clm/' :: prefix.
;; 2. [M-x clm/toggle-command-log-buffer] :: toggle command log buffer.

;; (require 'command-log-mode)
(autoload 'command-log-mode "command-log-mode" nil t)
(autoload 'clm/toggle-command-log-buffer "command-log-mode" nil t)

(setq command-log-mode-is-global t
      command-log-mode-auto-show t
      command-log-mode-key-binding-open-log "C-c t L"
      command-log-mode-open-log-turns-on-mode t
      command-log-mode-window-size 40
      command-log-mode-window-font-size 2
      )

;; (add-hook 'org-mode-hook 'command-log-mode)

(define-key my-tools-prefix-map (kbd "L") 'command-log-mode)


;;; [ mwe-log-commands ]


(provide 'init-my-tool-screencast)

;;; init-my-tool-screencast.el ends here
