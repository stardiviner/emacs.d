;;; init-emacs-comint.el --- init for Emacs comint
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Comint ]

(setq comint-prompt-read-only t
      comint-eol-on-send t ; go to the end of the line before sending input.
      comint-move-point-for-output t)

;;; translate ANSI color sequences into text properties.
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)

;;; strip ANSI color escape sequences.
;; (autoload 'ansi-color-for-comint-mode-filter "ansi-color" nil t)
;; (add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-filter)

(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

;; (add-hook 'comint-output-filter-functions 'comint-truncate-buffer)


(provide 'init-emacs-comint)

;;; init-emacs-comint.el ends here
