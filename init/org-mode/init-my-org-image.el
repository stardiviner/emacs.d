;;; init-my-org-image.el --- init for Org Image
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


;;; inline images
;;
;; [C-c C-x C-v] - `org-toggle-inline-images'.
;; [C-c C-x C-M-v] - `org-redisplay-inline-images'

(setq org-startup-with-inline-images nil
      org-image-actual-width 250        ; inline image scale width
      )



(provide 'init-my-org-image)

;;; init-my-org-image.el ends here
