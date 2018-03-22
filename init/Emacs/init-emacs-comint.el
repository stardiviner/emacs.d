;;; init-emacs-comint.el --- init for Emacs comint
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Comint ]

(setq comint-prompt-read-only t
      comint-eol-on-send t ; go to the end of the line before sending input.
      comint-move-point-for-output t
      )

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)

;; It will always put point back to the statement you entered, right above the
;; output it created.
;; (add-to-list 'comint-output-filter-functions
;;              #'(lambda (STR) (comint-show-output)))


(provide 'init-emacs-comint)

;;; init-emacs-comint.el ends here
