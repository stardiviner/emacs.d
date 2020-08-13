;;; init-prog-framework-arduino.el --- init Emacs for the Arduino language
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ arduino-mode ] -- major mode for Arduino.

(use-package arduino-mode
  :ensure t
  :defer t
  ;; :requires (flycheck-arduino)
  :commands (flycheck-arduino-setup)
  :hook (arduino-mode . flycheck-arduino-setup)
  :config
  ;; [ ob-arduino ] -- Org Mode Babel support for Arduino.
  (use-package org-plus-contrib
    :load-path "~/Code/Emacs/org-mode/contrib/lisp/"
    :no-require t
    :pin manual
    :init
    (use-package ob-arduino
      :defer t
      :after arduino-mode
      :commands (org-babel-execute:arduino)
      :config
      (add-to-list 'org-babel-load-languages '(arduino . t))
      (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
      (add-to-list 'org-babel-tangle-lang-exts '("arduino" . "ino")))))

;;; [ company-arduino ] -- code completion support for arduino-mode.

(use-package company-arduino
  :ensure t
  :ensure company-c-headers
  :defer t
  :after arduino-mode
  :preface
  (unless (getenv "ARDUINO_HOME")
    (setq company-arduino-home (setenv "ARDUINO_HOME" (expand-file-name "/usr/share/arduino"))))
  ;; Turn-on irony-mode on arduino-mode (on .ino file).
  (with-eval-after-load 'irony
    (add-to-list 'irony-supported-major-modes 'arduino-mode))
  :commands (company-arduino-turn-on)
  :hook ((arduino-mode . irony-mode)
         (arduino-mode . company-arduino-turn-on))
  :config
  (defun my/company-arduino-setup ()
    (my-company-add-backend-locally 'company-irony)

    ;; (setq-local irony-arduino-includes-options)
    
    ;; temporary workaround for Arduino v17 new libraries location.
    ;; (setq-local company-arduino-includes-dirs '())
    ;; (add-to-list 'company-arduino-includes-dirs '("~/Arduino/libraries/"))
    ;; (add-to-list 'company-backends 'company-c-headers)
    )
  (add-hook 'arduino-mode-hook #'my/company-arduino-setup))

;;; [ arduino-cli-mode ] -- Emacs support for the arduino-cli.

(use-package arduino-cli-mode ; [C-c C-a]
  :ensure t
  :defer t
  :commands (arduino-cli-board-list)
  :hook (arduino-mode . arduino-cli-mode))

;;; [ platformio-mode ] -- PlatformIO integration for Emacs

(use-package platformio-mode
  :ensure t
  :defer t
  :hook (c++-mode . platformio-conditionally-enable))


(provide 'init-prog-framework-arduino)

;;; init-prog-framework-arduino.el ends here
