;;; init-my-prog-document-man.el --- init for man/women lookup commands.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;; (unless (boundp 'man-prefix)
;;   (define-prefix-command 'man-prefix))
;; (define-key prog-doc-map (kbd "m") 'man-prefix)

(global-set-key (kbd "C-h u") 'manual-entry) ; Unix man pages
(define-key prog-doc-map (kbd "m") 'manual-entry)

(add-to-list 'display-buffer-alist
             '("\\*Man.*\\*" (display-buffer-below-selected)))

;;; [ Man ]

;; (use-package man
;;   :bind (:map prog-doc-map
;;               ("m" . man-follow)
;;               ("M" . man))
;;   )

;;; [ women ]

;; (use-package woman
;;   :bind (:map prog-doc-map
;;               ("M-m" . woman))
;;   )


(provide 'init-my-prog-document-man)

;;; init-my-prog-document-man.el ends here
