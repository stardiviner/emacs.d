;;; init-org-image.el --- init for Org Image
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ inline images ]

(setq org-image-actual-width nil)

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
(defun create-image-with-background-color (args)
  "Specify background color of Org-mode inline image through modify `ARGS'."
  (let* ((file (car args))
         (type (cadr args))
         (data-p (caddr args))
         (props (cdddr args)))
    ;; get this return result style from `create-image'
    (append (list file type data-p)
            (list :background (face-background 'default)) ; or :background "white".
            props)))

(advice-add 'create-image :filter-args #'create-image-with-background-color)

;;; insert Org inline image without description automatically detect link image extension.
(defun my/org-insert-link-as-inline-image (orig-func link &optional description)
  (if (member (file-name-extension link) image-file-name-extensions)
      (setq description nil))
  (funcall orig-func link description))

(advice-add 'org-link-make-string :around #'my/org-insert-link-as-inline-image)

;;; Only display inline images under current subtree.
(defun org-display-subtree-inline-images ()
  "Toggle the display of inline images under current subtree.
INCLUDE-LINKED is passed to `org-display-inline-images'."
  (interactive)
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (let* ((beg (point-min))
             (end (point-max))
             (image-overlays (cl-intersection
                              org-inline-image-overlays
                              (overlays-in beg end))))
        (if image-overlays
            (progn
              (org-remove-inline-images)
              (message "Inline image display turned off"))
          (org-display-inline-images t t beg end)
          (setq image-overlays (cl-intersection
                                org-inline-image-overlays
                                (overlays-in beg end)))
          (if (and (org-called-interactively-p) image-overlays)
              (message "%d images displayed inline"
                       (length image-overlays))))))))

(define-key org-mode-map (kbd "C-c C-x C-v") 'org-display-subtree-inline-images)



(provide 'init-org-image)

;;; init-org-image.el ends here
