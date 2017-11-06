;;; init-my-prog-framework-arduino.el --- init Emacs for the Arduino language
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ arduino-mode ]

(use-package arduino-mode
  :ensure t
  :defer t
  :config
  (setq arduino-font-lock-extra-types t)
  )


;;; [ company-arduino ]

(use-package company-arduino
  :ensure t
  :init
  (setenv "ARDUINO_HOME" "/usr/share/arduino")
  
  (add-hook 'arduino-mode-hook #'irony-mode)
  (add-hook 'arduino-mode-hook #'company-arduino-turn-on)
  :config
  ;; (defun my-company-c-headers-get-system-path ()
  ;;   "Return the system include path for the current buffer."
  ;;   (let ((default '("/usr/include/" "/usr/local/include/")))
  ;;     (company-arduino-append-include-dirs default t)))
  ;; (setq company-c-headers-path-system 'my-company-c-headers-get-system-path)
  )


(provide 'init-my-prog-framework-arduino)

;;; init-my-prog-framework-arduino.el ends here
