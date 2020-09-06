;;; init-prog-document-man.el --- init for man/women lookup commands.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Man ]

(use-package man
  :bind (:map document-prefix ("m" . man) ("M" . man-follow))
  :init (add-to-list 'display-buffer-alist '("^\\*Man .*\\*\\'" . (display-buffer-below-selected))))

;;; [ women ]

;; (use-package woman
;;   :bind (:map document-prefix ("M-m" . woman)))


(provide 'init-prog-document-man)

;;; init-prog-document-man.el ends here
