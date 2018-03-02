;;; init-my-prog-framework-arduino.el --- init Emacs for the Arduino language
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ arduino-mode ]

(use-package arduino-mode
  :ensure t
  :ensure-system-package arduino
  :defer t
  :config
  (require 'flycheck-arduino)
  (add-hook 'arduino-mode-hook #'flycheck-arduino-setup)
  )

;;; [ ob-arduino ]

(use-package org-plus-contrib
  :load-path "~/Code/Emacs/org-mode/contrib/lisp/"
  :no-require t
  :pin manual
  :ensure-system-package arduino
  :load (ob-arduino)
  :init
  (add-to-list 'org-babel-load-languages '(arduino . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  :config
  (add-to-list 'org-babel-tangle-lang-exts '("arduino" . "ino"))
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
