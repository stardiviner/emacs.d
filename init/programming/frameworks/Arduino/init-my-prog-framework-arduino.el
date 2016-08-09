;;; init-my-prog-framework-arduino.el --- init Emacs for the Arduino language
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ arduino-mode ]

;; (use-package arduino-mode
;;   :ensure t
;;   :config
;;   (setq arduino-font-lock-extra-types t)
;;   )


;;; [ company-arduino ]

(use-package company-arduino
  :ensure t
  :init
  (setenv "ARDUINO_HOME" "/usr/share/arduino")
  :config
  (add-hook 'arduino-mode-hook
            (lambda ()
              (irony-mode 1)
              (company-arduino-turn-on)
              ;; (add-to-list 'company-backends 'company-irony)
              ;; (add-to-list 'company-backends 'company-c-headers)
              ))
  
  ;; (defun my-company-c-headers-get-system-path ()
  ;;   "Return the system include path for the current buffer."
  ;;   (let ((default '("/usr/include/" "/usr/local/include/")))
  ;;     (company-arduino-append-include-dirs default t)))
  ;; (setq company-c-headers-path-system 'my-company-c-headers-get-system-path)
  )


(provide 'init-my-prog-framework-arduino)

;;; init-my-prog-framework-arduino.el ends here
