;;; init-my-prog-framework-arduino.el --- init Emacs for the Arduino language
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ arduino-mode ]

(use-package arduino-mode
  :ensure-system-package arduino
  ;; :ensure t
  :quelpa (arduino-mode :fetcher github :repo "stardiviner/arduino-mode" :upgrade t)
  ;; :load-path "~/Code/Emacs/arduino-mode"
  ;; :config
  ;; (setq arduino-font-lock-extra-types '()) ; this depend on c-font-lock like c-lang-defvar etc.
  )


;;; [ company-arduino ]

(use-package company-arduino
  ;; :ensure t
  :load-path "~/Code/Emacs/company-arduino"
  :after arduino-mode
  :preface
  (setq company-arduino-home
        (setenv "ARDUINO_HOME" (expand-file-name "~/Arduino/")))
  :init
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
