;;; init-my-prog-lang-swift.el --- init for Swift
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ swift-mode ]

;;; Usage:
;;
;; -

(require 'swift-mode)

;; Swift flycheck is disabled by default because not available under Linux.
(setq flycheck-swift-sdk-path "")
;; (add-to-list 'flycheck-checkers 'swift)


(provide 'init-my-prog-lang-swift)

;;; init-my-prog-lang-swift.el ends here
