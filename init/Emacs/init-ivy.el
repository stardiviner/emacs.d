;;; init-ivy.el --- init for Ivy-mode
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ swiper/ivy-mode ]

(use-package swiper
  :config
  (global-set-key (kbd "C-s") 'swiper)
  ;; Ivy-mode
  (setq ivy-use-virtual-buffers t
        ivy-height 7
        ivy-count-format "(%d/%d) "
        ivy-wrap nil
        )
  (ivy-mode 1)
  )


;;; [ counsel ]

(use-package counsel)


(provide 'init-ivy)

;;; init-ivy.el ends here
