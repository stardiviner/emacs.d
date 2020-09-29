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

;;; auto display inline images under current TAB cycle expanded "visible" subtree.
(defun org-display-subtree-with-inline-images (&optional state)
  "Toggle the display of inline images under current expanded visible subtree.
INCLUDE-LINKED is passed to `org-display-inline-images'."
  (interactive)
  (when (and (display-graphic-p)
             (derived-mode-p 'org-mode)
             (memq state '(children subtree))
             ;; (not (memq state '(overview folded contents)))
             )
    (save-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (let* ((beg (point-min))
               (end (save-excursion (org-next-visible-heading 1) (point)))
               (image-overlays (cl-intersection
                                org-inline-image-overlays
                                (overlays-in beg end)))
               (display-inline-images-local
                (lambda ()
                  (org-display-inline-images t t beg end)
                  (setq image-overlays (cl-intersection
                                        org-inline-image-overlays
                                        (overlays-in beg end)))
                  (if (and (org-called-interactively-p) image-overlays)
                      (message "%d images displayed inline"
                               (length image-overlays)))))
               (hide-inline-images-local
                (lambda ()
                  (org-remove-inline-images)
                  (message "Inline image display turned off"))))
          (if state
              (pcase state
                ('subtree
                 (funcall display-inline-images-local))
                ('children
                 (funcall display-inline-images-local))
                ('folded
                 (funcall hide-inline-images-local)))
            (if image-overlays
                (funcall hide-inline-images-local)
              (funcall display-inline-images-local))))))))

(add-hook 'org-cycle-hook #'org-display-subtree-with-inline-images)

(define-key org-mode-map (kbd "C-c C-x C-v") 'org-display-subtree-with-inline-images)



(provide 'init-org-image)

;;; init-org-image.el ends here
