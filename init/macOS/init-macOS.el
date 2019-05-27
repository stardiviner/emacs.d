;;; init-macOS.el --- init for Mac OS X
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; For MacOS
(setq mac-option-modifier 'hyper) ; sets the Option key as Hyper
(setq mac-option-modifier 'super) ; sets the Option key as Super
(setq mac-command-modifier 'meta) ; sets the Command key as Meta
(setq mac-control-modifier 'meta) ; sets the Control key as Meta

;;; [ company-xcode ]

(defun my-xcode-setup ()
  (require 'company-xcode)
  (my-company-add-backend-locally 'company-xcode))

;;; [ counsel-osx-app ]

(use-package counsel-osx-app
  :ensure t
  :defer t)

;;; [ helm-xcdoc ] -- Search Xcode Document by docsetutil and eww with Helm interface.

(use-package helm-xcdoc
  :ensure t)


(provide 'init-macOS)

;;; init-macOS.el ends here
