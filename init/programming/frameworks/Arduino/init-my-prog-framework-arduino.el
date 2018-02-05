;;; init-my-prog-framework-arduino.el --- init Emacs for the Arduino language
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ arduino-mode ]

(use-package arduino-mode
  ;; :ensure t
  :ensure-system-package arduino
  ;; TODO: switch to official package after the maintainer accepted new PRs.
  ;; https://github.com/bookest/arduino-mode/network
  :preface
  ;; fix bug (void-function ede-project-autoload)
  (require 'ede)
  (defvar ede-arduino-preferences-file (expand-file-name "~/.arduino/preferences.txt"))
  
  :defines ede-arduino-preferences-file
  :quelpa (arduino-mode
           :fetcher github
           :repo "bwachter/arduino-mode"
           :upgrade t) ; "bwachter/arduino-mode", "mavit/arduino-mode"
  :ensure ede
  :bind (:map arduino-mode-map
              ("C-c C-c" . arduino-upload))
  ;; :config
  ;; (setq arduino-font-lock-extra-types '()) ; this depend on c-font-lock like c-lang-defvar etc.
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
