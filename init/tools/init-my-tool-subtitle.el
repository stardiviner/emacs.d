;;; init-my-tool-subtitle.el --- init for Editing Subtitle
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Simple Emacs-based Subtitle Editor (SESE) ]

;; (load "~/.emacs.d/init/extensions/sese.el")
(autoload 'sese-mode "sese" "Subtitle Editor major mode" t)

(setq auto-mode-alist
      (cons '("\\.sese\\'" . sese-mode) auto-mode-alist))



(provide 'init-my-tool-subtitle)

;;; init-my-tool-subtitle.el ends here
