;;; init-my-prog-lang-xml.el --- init XML programming language
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ nxml-mode ]

(require 'nxml-mode)

(push '("<\\?xml" . nxml-mode) magic-mode-alist)

;; pom files should be treated as xml files
(add-to-list 'auto-mode-alist '("\\.pom$" . nxml-mode))

(setq nxml-child-indent 4
      nxml-attribute-indent 4
      nxml-auto-insert-xml-declaration-flag nil
      nxml-bind-meta-tab-to-complete-flag t
      nxml-slash-auto-complete-flag t)


;;; [ auto-complete-nxml ]

;;; https://github.com/aki2o/auto-complete-nxml

;; (require 'auto-complete-nxml)

;; ;; If you want to start completion manually from the beginning
;; (setq auto-complete-nxml-automatic-p nil)

;; ;; Keystroke for popup help about something at point.
;; (setq auto-complete-nxml-popup-help-key "M-h")

;; ;; Keystroke for toggle on/off automatic completion.
;; ;; (setq auto-complete-nxml-toggle-automatic-key "C-c C-t")



(provide 'init-my-prog-lang-xml)

;;; init-my-prog-lang-xml.el ends here
