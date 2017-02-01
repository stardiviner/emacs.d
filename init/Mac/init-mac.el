;;; init-mac.el --- init for Mac OS X
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; For MacOS
(setq mac-option-modifier 'hyper) ; sets the Option key as Hyper
(setq mac-option-modifier 'super) ; sets the Option key as Super
(setq mac-command-modifier 'meta) ; sets the Command key as Meta
(setq mac-control-modifier 'meta) ; sets the Control key as Meta

;;; [ counsel-osx-app ]

(use-package counsel-osx-app
  :ensure t
  :defer t)


(provide 'init-mac)

;;; init-mac.el ends here
