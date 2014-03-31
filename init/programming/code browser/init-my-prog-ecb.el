;;; init-my-prog-ecb.el --- init ECB
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ ECB ]

(require 'ecb)

;; alternative: This is a smarter way when you need semantic only if ECB is active.
(add-hook 'ecb-before-activate-hook
          (lambda () (semantic-mode 1)))

;;; active ECB

;; (ecb-activate)
;; or
;; (ecb-minor-mode)

;; (ecb-show-help)



(provide 'init-my-prog-ecb)

;;; init-my-prog-ecb.el ends here
