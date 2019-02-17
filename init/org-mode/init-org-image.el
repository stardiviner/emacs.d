;;; init-org-image.el --- init for Org Image
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ inline images ]

;; PERFORMANCE: disable starup preview inline image to improve org-mode pferformance.
(setq org-startup-with-inline-images t)

;;; manually specify inline image size.
;; you need to use:
;; - #+ATTR_ORG: :width 400
;; - #+ATTR_LATEX: :width 5in
;; - #+ATTR_HTML: :width 200px
(use-package org
  :ensure-system-package (convert . "sudo pacman -S --noconfirm imagemagick")
  :init
  ;; inline image scale width.
  (setq org-image-actual-width nil))

;;; smartly set inline image size based on Emacs window width
;; (defcustom org-inline-image-width-percent-in-window 80
;;   "How much percent of current window width for org-mode inline image size."
;;   :type 'number
;;   :group 'org-image)
;;
;; (defun org-inline-image-width-smart ()
;;   "Smartly set inline image size based on percent of Emacs window width."
;;   (let* ((max-width (window-width (selected-window) t))
;;          (percent-width (floor (* max-width (/ (float org-inline-image-width-percent-in-window) 100)))))
;;     ;; TODO: detect image width, only if it is wider than window width.
;;     (setq-local org-image-actual-width percent-width)))
;;
;; (add-hook 'org-mode-hook #'org-inline-image-width-smart 'append)


;;; set default Org-mode inline image background color based on color-theme background color.
(defun org-display-inline-images--with-color-theme-background-color (args)
  "Specify background color of Org-mode inline image through modify `ARGS'."
  (let* ((file (car args))
         (type (cadr args))
         (data-p (caddr args))
         (props (cdddr args)))
    ;; get this return result style from `create-image'
    (append (list file type data-p)
            (list :background (face-background 'default))
            props)))

(advice-add 'create-image :filter-args
            #'org-display-inline-images--with-color-theme-background-color)



(provide 'init-org-image)

;;; init-org-image.el ends here
