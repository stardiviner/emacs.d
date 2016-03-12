;;; init-my-emacs-comint.el --- init for Emacs comint
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Comint ]

(setq comint-prompt-read-only t
      comint-eol-on-send t ; go to the end of the line before sending input.
      comint-move-point-for-output t
      )


(provide 'init-my-emacs-comint)

;;; init-my-emacs-comint.el ends here
