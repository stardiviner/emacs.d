;;; init-my-prog-document-man.el --- init for man/women lookup commands.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'man-lookup-prefix)
  (define-prefix-command 'man-lookup-prefix))
;; (define-key help-document-map (kbd "m") 'man-lookup-prefix)


;;; [ Man ]

;;; Usage:
;; - Man-?? :: command prefix.


(if (featurep 'helm)
    (define-key help-document-map (kbd "m") 'helm-man-woman))



;;; [ women ]




;;; [ iman ]




;;; [ man-preview ]



(provide 'init-my-prog-document-man)

;;; init-my-prog-document-man.el ends here
