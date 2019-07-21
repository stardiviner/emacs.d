;;; init-prog-framework-arduino.el --- init Emacs for the Arduino language
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ arduino-mode ]

(use-package arduino-mode
  :ensure t
  :defer t
  :commands (arduino-mode flycheck-arduino-setup)
  :config (add-hook 'arduino-mode-hook #'flycheck-arduino-setup))

;;; [ ob-arduino ]

(use-package org-plus-contrib
  :load-path "~/Code/Emacs/org-mode/contrib/lisp/"
  :no-require t
  :pin manual
  :init
  (use-package ob-arduino
    :defer t
    :commands (org-babel-execute:arduino)
    :config
    (add-to-list 'org-babel-load-languages '(arduino . t))
    (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
    (add-to-list 'org-babel-tangle-lang-exts '("arduino" . "ino"))))

;;; [ company-arduino ]

(use-package company-arduino
  ;; :ensure t
  :ensure company-c-headers
  :load-path "~/Code/Emacs/company-arduino"
  :defer t
  :after arduino-mode
  :preface (setq company-arduino-home
                 (setenv "ARDUINO_HOME" (expand-file-name "~/Arduino/")))
  :commands (company-arduino-turn-on)
  :init
  ;; Turn-on irony-mode on arduino-mode (on .ino file).
  (add-hook 'arduino-mode-hook #'irony-mode)
  (add-hook 'arduino-mode-hook #'company-arduino-turn-on)
  :config
  ;; temporary workaround for Arduino v17 new libraries location.
  (add-hook 'arduino-mode-hook
            (lambda ()
              (setq company-arduino-includes-dirs '("~/Arduino/libraries/"))
              (my-company-add-backend-locally 'company-c-headers))))


(provide 'init-prog-framework-arduino)

;;; init-prog-framework-arduino.el ends here
