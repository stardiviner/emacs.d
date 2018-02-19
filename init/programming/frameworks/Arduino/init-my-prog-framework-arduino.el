;;; init-my-prog-framework-arduino.el --- init Emacs for the Arduino language
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ arduino-mode ]

(use-package arduino-mode
  :ensure t
  :ensure-system-package arduino
  ;; :load-path "~/Code/Emacs/arduino-mode"
  :defer t
  )


;;; [ company-arduino ]

(use-package company-arduino
  ;; :ensure t
  :defer t
  :load-path "~/Code/Emacs/company-arduino"
  :after arduino-mode
  :preface
  (setq company-arduino-home
        (setenv "ARDUINO_HOME" (expand-file-name "~/Arduino/")))
  :init
  ;; Turn-on irony-mode on arduino-mode (on .ino file).
  (add-hook 'arduino-mode-hook #'irony-mode)
  (add-hook 'arduino-mode-hook #'company-arduino-turn-on)
  :config
  ;; temporary workaround for Arduino v17 new libraries location.
  ;; (setq company-arduino-includes-dirs '("~/Arduino/libraries/"))
  )


(provide 'init-my-prog-framework-arduino)

;;; init-my-prog-framework-arduino.el ends here
