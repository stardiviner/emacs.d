;;; init-my-prog-document-man.el --- init for man/women lookup commands.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'man-lookup-prefix)
  (define-prefix-command 'man-lookup-prefix))
;; (define-key my-prog-help-document-map (kbd "m") 'man-lookup-prefix)


;;; [ Man ]

;;; Usage:
;; - Man-?? :: command prefix.

(define-key my-prog-help-document-map (kbd "m") 'man-follow)
(define-key my-prog-help-document-map (kbd "M") 'man)


;;; [ women ]


;;; [ iman ]


;;; [ man-preview ]


(provide 'init-my-prog-document-man)

;;; init-my-prog-document-man.el ends here
