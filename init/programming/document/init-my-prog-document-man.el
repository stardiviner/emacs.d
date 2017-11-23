;;; init-my-prog-document-man.el --- init for man/women lookup commands.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'man-lookup-prefix)
  (define-prefix-command 'man-lookup-prefix))
;; (define-key prog-doc-map (kbd "m") 'man-lookup-prefix)


;;; [ Man ]

(use-package man
  :bind (:map prog-doc-map
              ("m" . man-follow)
              ("M" . man))
  )

;;; [ women ]

(use-package woman
  :bind (:map prog-doc-map
              ("M-m" . woman))
  )


(provide 'init-my-prog-document-man)

;;; init-my-prog-document-man.el ends here
