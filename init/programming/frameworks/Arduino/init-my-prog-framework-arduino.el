;;; init-my-prog-framework-arduino.el --- init Emacs for the Arduino language
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ arduino-mode ]

(use-package arduino-mode
  :ensure t
  :config
  (setq arduino-font-lock-extra-types t)
  )


(provide 'init-my-prog-framework-arduino)

;;; init-my-prog-framework-arduino.el ends here
