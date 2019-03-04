;;; init-prog-document-man.el --- init for man/women lookup commands.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;; (unless (boundp 'man-prefix)
;;   (define-prefix-command 'man-prefix))
;; (define-key document-prefix (kbd "m") 'man-prefix)

(define-key document-prefix (kbd "m") 'manual-entry)

(add-to-list 'display-buffer-alist
             '("\\*Man.*\\*" (display-buffer-below-selected)))

;;; [ Man ]

;; (use-package man
;;   :bind (:map document-prefix
;;               ("m" . man-follow)
;;               ("M" . man))
;;   )

;;; [ women ]

;; (use-package woman
;;   :bind (:map document-prefix
;;               ("M-m" . woman))
;;   )


(provide 'init-prog-document-man)

;;; init-prog-document-man.el ends here
