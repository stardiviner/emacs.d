;;; init-tool-subtitle.el --- init for Editing Subtitle
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Simple Emacs-based Subtitle Editor (SESE) ]

;; (load "~/.emacs.d/init/extensions/sese.el")
(autoload 'sese-mode "sese" "Subtitle Editor major mode" t)

(setq auto-mode-alist
      (cons '("\\.sese\\'" . sese-mode) auto-mode-alist))



(provide 'init-tool-subtitle)

;;; init-tool-subtitle.el ends here
