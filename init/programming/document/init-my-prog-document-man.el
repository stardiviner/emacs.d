;;; init-my-prog-document-man.el --- init for man/women lookup commands.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'man-lookup-prefix)
  (define-prefix-command 'man-lookup-prefix))
;; (define-key my-prog-help-document-map (kbd "m") 'man-lookup-prefix)


;;; [ Man ]

(use-package man
  :bind (:map my-prog-help-document-map
              ("m" . man-follow)
              ("M" . man))
  :config
  (set-face-attribute 'Man-overstrike nil
                      :inherit font-lock-type-face
                      :bold t)
  (set-face-attribute 'Man-underline nil
                      :inherit font-lock-keyword-face
                      :underline t)
  )

;;; [ women ]


;;; [ iman ]


;;; [ man-preview ]


(provide 'init-my-prog-document-man)

;;; init-my-prog-document-man.el ends here
